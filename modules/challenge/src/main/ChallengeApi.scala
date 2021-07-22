package lishogi.challenge

import org.joda.time.DateTime
import scala.concurrent.duration._

import lishogi.common.Bus
import lishogi.common.config.Max
import lishogi.game.{ Game, Pov }
import lishogi.hub.actorApi.socket.SendTo
import lishogi.user.{ User, UserRepo }
import lishogi.memo.CacheApi._

final class ChallengeApi(
    repo: ChallengeRepo,
    challengeMaker: ChallengeMaker,
    userRepo: UserRepo,
    joiner: Joiner,
    jsonView: JsonView,
    gameCache: lishogi.game.Cached,
    maxPlaying: Max,
    cacheApi: lishogi.memo.CacheApi
)(implicit
    ec: scala.concurrent.ExecutionContext,
    system: akka.actor.ActorSystem
) {

  import Challenge._

  def allFor(userId: User.ID): Fu[AllChallenges] =
    createdByDestId(userId) zip createdByChallengerId(userId) dmap (AllChallenges.apply _).tupled

  // returns boolean success
  def create(c: Challenge): Fu[Boolean] =
    isLimitedByMaxPlaying(c) flatMap {
      case true => fuFalse
      case false =>
        {
          repo like c flatMap { _ ?? repo.cancel }
        } >> (repo insert c) >>- {
          uncacheAndNotify(c)
          Bus.publish(Event.Create(c), "challenge")
        } inject true
    }

  def byId = repo byId _

  def activeByIdFor(id: Challenge.ID, dest: User) = repo.byIdFor(id, dest).dmap(_.filter(_.active))

  def onlineByIdFor(id: Challenge.ID, dest: User) = repo.byIdFor(id, dest).dmap(_.filter(_.online))

  val countInFor = cacheApi[User.ID, Int](65536, "challenge.countInFor") {
    _.expireAfterAccess(20 minutes)
      .buildAsyncFuture(repo.countCreatedByDestId)
  }

  def createdByChallengerId = repo createdByChallengerId _

  def createdByDestId = repo createdByDestId _

  def cancel(c: Challenge) = (repo cancel c) >>- uncacheAndNotify(c)

  private def offline(c: Challenge) = (repo offline c) >>- uncacheAndNotify(c)

  private[challenge] def ping(id: Challenge.ID): Funit =
    repo statusById id flatMap {
      case Some(Status.Created) => repo setSeen id
      case Some(Status.Offline) => (repo setSeenAgain id) >> byId(id).map { _ foreach uncacheAndNotify }
      case _                    => fuccess(socketReload(id))
    }

  def decline(c: Challenge) = (repo decline c) >>- uncacheAndNotify(c)

  private val acceptQueue = new lishogi.hub.DuctSequencer(maxSize = 64, timeout = 5 seconds, "challengeAccept")

  def accept(
      c: Challenge,
      user: Option[User],
      sid: Option[String],
      color: Option[shogi.Color] = None
  ): Fu[Option[Pov]] =
    acceptQueue {
      if (c.challengerIsOpen)
        repo.setChallenger(c.setChallenger(user, sid), color) inject none
      else
        joiner(c, user, color).flatMap {
          _ ?? { pov =>
            (repo accept c) >>- {
              uncacheAndNotify(c)
              Bus.publish(Event.Accept(c, user.map(_.id)), "challenge")
            } inject pov.some
          }
        }
    }

  def sendRematchOf(game: Game, user: User): Fu[Boolean] =
    challengeMaker.makeRematchOf(game, user) flatMap { _ ?? create }

  def setDestUser(c: Challenge, u: User): Funit = {
    val challenge = c setDestUser u
    repo.update(challenge) >>- {
      uncacheAndNotify(challenge)
      Bus.publish(Event.Create(challenge), "challenge")
    }
  }

  def removeByUserId(userId: User.ID) =
    repo allWithUserId userId flatMap { cs =>
      lishogi.common.Future.applySequentially(cs)(remove).void
    }

  def oauthAccept(dest: User, challenge: Challenge): Fu[Option[Game]] =
    joiner(challenge, dest.some, none).map2(_.game)

  private def isLimitedByMaxPlaying(c: Challenge) =
    if (c.hasClock) fuFalse
    else
      c.userIds
        .map { userId =>
          gameCache.nbPlaying(userId) dmap (maxPlaying <=)
        }
        .sequenceFu
        .dmap(_ exists identity)

  private[challenge] def sweep: Funit =
    repo.realTimeUnseenSince(DateTime.now minusSeconds 20, max = 50).flatMap { cs =>
      lishogi.common.Future.applySequentially(cs)(offline).void
    } >>
      repo.expired(50).flatMap { cs =>
        lishogi.common.Future.applySequentially(cs)(remove).void
      }

  private def remove(c: Challenge) =
    repo.remove(c.id) >>- uncacheAndNotify(c)

  private def uncacheAndNotify(c: Challenge): Unit = {
    c.destUserId ?? countInFor.invalidate
    c.destUserId ?? notify
    c.challengerUserId ?? notify
    socketReload(c.id)
  }

  private def socketReload(id: Challenge.ID): Unit =
    socket foreach (_ reload id)

  private def notify(userId: User.ID): Funit =
    for {
      all <- allFor(userId)
      lang <- userRepo langOf userId map {
        _ flatMap lishogi.i18n.I18nLangPicker.byStr getOrElse lishogi.i18n.defaultLang
      }
    } yield Bus.publish(
      SendTo(userId, lishogi.socket.Socket.makeMessage("challenges", jsonView(all)(lang))),
      "socketUsers"
    )

  // work around circular dependency
  private var socket: Option[ChallengeSocket] = None
  private[challenge] def registerSocket(s: ChallengeSocket) = { socket = s.some }
}
