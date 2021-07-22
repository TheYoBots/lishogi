package lishogi.lobby

import org.joda.time.DateTime
import scala.annotation.nowarn
import scala.concurrent.duration._
import scala.concurrent.Promise

import actorApi._
import lishogi.common.config.Max
import lishogi.common.{ AtMost, Bus, Every }
import lishogi.game.Game
import lishogi.hub.Trouper
import lishogi.socket.Socket.{ Sri, Sris }
import lishogi.user.User

final private class LobbyTrouper(
    seekApi: SeekApi,
    biter: Biter,
    gameCache: lishogi.game.Cached,
    maxPlaying: Max,
    playbanApi: lishogi.playban.PlaybanApi,
    poolApi: lishogi.pool.PoolApi,
    onStart: lishogi.round.OnStart
)(implicit ec: scala.concurrent.ExecutionContext)
    extends Trouper {

  import LobbyTrouper._

  private var remoteDisconnectAllAt = DateTime.now

  private var socket: Trouper = Trouper.stub

  val process: Trouper.Receive = {

    // solve circular reference
    case SetSocket(trouper) => socket = trouper

    case msg @ AddHook(hook) =>
      lishogi.mon.lobby.hook.create.increment()
      HookRepo bySri hook.sri foreach remove
      hook.sid ?? { sid =>
        HookRepo bySid sid foreach remove
      }
      !hook.compatibleWithPools ?? findCompatible(hook) match {
        case Some(h) => biteHook(h.id, hook.sri, hook.user)
        case None =>
          HookRepo save msg.hook
          socket ! msg
      }

    case msg @ AddSeek(seek) =>
      lishogi.mon.lobby.seek.create.increment()
      findCompatible(seek) foreach {
        case Some(s) => this ! BiteSeek(s.id, seek.user)
        case None    => this ! SaveSeek(msg)
      }

    case SaveSeek(msg) =>
      (seekApi insert msg.seek) >>- {
        socket ! msg
      }

    case CancelHook(sri) =>
      HookRepo bySri sri foreach remove

    case CancelSeek(seekId, user) =>
      seekApi.removeBy(seekId, user.id) >>- {
        socket ! RemoveSeek(seekId)
      }

    case BiteHook(hookId, sri, user) =>
      NoPlayban(user) {
        biteHook(hookId, sri, user)
      }

    case BiteSeek(seekId, user) =>
      NoPlayban(user.some) {
        gameCache.nbPlaying(user.id) foreach { nbPlaying =>
          if (maxPlaying > nbPlaying) {
            lishogi.mon.lobby.seek.join.increment()
            seekApi find seekId foreach {
              _ foreach { seek =>
                biter(seek, user) foreach this.!
              }
            }
          }
        }
      }

    case msg @ JoinHook(_, hook, game, _) =>
      onStart(game.id)
      socket ! msg
      remove(hook)

    case msg @ JoinSeek(_, seek, game, _) =>
      onStart(game.id)
      socket ! msg
      seekApi.archive(seek, game.id) >>- {
        socket ! RemoveSeek(seek.id)
      }

    case LeaveAll => remoteDisconnectAllAt = DateTime.now

    case Tick(promise) =>
      HookRepo.truncateIfNeeded()
      socket
        .ask[Sris](GetSrisP)
        .chronometer
        .logIfSlow(100, logger) { r =>
          s"GetSris size=${r.sris.size}"
        }
        .mon(_.lobby.socket.getSris)
        .result
        .logFailure(logger, err => s"broom cannot get sris from socket: $err")
        .foreach { this ! WithPromise(_, promise) }

    case WithPromise(Sris(sris), promise) =>
      poolApi socketIds Sris(sris)
      val fewSecondsAgo = DateTime.now minusSeconds 5
      if (remoteDisconnectAllAt isBefore fewSecondsAgo) this ! RemoveHooks({
        (HookRepo notInSris sris).filter { h =>
          !h.boardApi && (h.createdAt isBefore fewSecondsAgo)
        } ++ HookRepo.cleanupOld
      }.toSet)
      lishogi.mon.lobby.socket.member.update(sris.size)
      lishogi.mon.lobby.hook.size.record(HookRepo.size)
      lishogi.mon.trouper.queueSize("lobby").update(queueSize)
      promise.success(())

    case RemoveHooks(hooks) => hooks foreach remove

    case Resync => socket ! HookIds(HookRepo.vector.map(_.id))

    case HookSub(member, true) =>
      socket ! AllHooksFor(member, HookRepo.vector.filter { biter.showHookTo(_, member) })

    case lishogi.pool.HookThieve.GetCandidates(clock, promise) =>
      promise success lishogi.pool.HookThieve.PoolHooks(HookRepo poolCandidates clock)

    case lishogi.pool.HookThieve.StolenHookIds(ids) =>
      HookRepo byIds ids.toSet foreach remove
  }

  private def NoPlayban(user: Option[LobbyUser])(f: => Unit): Unit = {
    user.?? { u =>
      playbanApi.currentBan(u.id)
    } foreach {
      case None => f
      case _    =>
    }
  }

  private def biteHook(hookId: String, sri: Sri, user: Option[LobbyUser]) =
    HookRepo byId hookId foreach { hook =>
      remove(hook)
      HookRepo bySri sri foreach remove
      biter(hook, sri, user) foreach this.!
    }

  private def findCompatible(hook: Hook): Option[Hook] =
    findCompatibleIn(hook, HookRepo findCompatible hook)

  private def findCompatibleIn(hook: Hook, in: Vector[Hook]): Option[Hook] =
    in match {
      case Vector() => none
      case h +: rest =>
        if (
          biter.canJoin(h, hook.user) && !(
            (h.user |@| hook.user).tupled ?? { case (u1, u2) =>
              recentlyAbortedUserIdPairs.exists(u1.id, u2.id)
            }
          )
        ) h.some
        else findCompatibleIn(hook, rest)
    }

  def registerAbortedGame(g: Game) = recentlyAbortedUserIdPairs register g

  private object recentlyAbortedUserIdPairs {
    private val cache                                     = new lishogi.memo.ExpireSetMemo(1 hour)
    private def makeKey(u1: User.ID, u2: User.ID): String = if (u1 < u2) s"$u1/$u2" else s"$u2/$u1"
    @nowarn("cat=unused") def register(g: Game) =
      for {
        sp <- g.sentePlayer.userId
        gp <- g.gotePlayer.userId
        if g.fromLobby
      } cache.put(makeKey(sp, gp))
    def exists(u1: User.ID, u2: User.ID) = cache.get(makeKey(u1, u2))
  }

  private def findCompatible(seek: Seek): Fu[Option[Seek]] =
    seekApi forUser seek.user map {
      _ find (_ compatibleWith seek)
    }

  private def remove(hook: Hook) = {
    HookRepo remove hook
    socket ! RemoveHook(hook.id)
    Bus.publish(RemoveHook(hook.id), s"hookRemove:${hook.id}")
  }
}

private object LobbyTrouper {

  case class SetSocket(trouper: Trouper)

  private case class Tick(promise: Promise[Unit])

  private case class WithPromise[A](value: A, promise: Promise[Unit])

  def start(
      broomPeriod: FiniteDuration,
      resyncIdsPeriod: FiniteDuration
  )(
      makeTrouper: () => LobbyTrouper
  )(implicit ec: scala.concurrent.ExecutionContext, system: akka.actor.ActorSystem) = {
    val trouper = makeTrouper()
    Bus.subscribe(trouper, "lobbyTrouper")
    system.scheduler.scheduleWithFixedDelay(15 seconds, resyncIdsPeriod)(() => trouper ! actorApi.Resync)
    lishogi.common.ResilientScheduler(
      every = Every(broomPeriod),
      atMost = AtMost(10 seconds),
      initialDelay = 7 seconds
    ) { trouper.ask[Unit](Tick) }
    trouper
  }
}
