package lishogi.challenge

import org.joda.time.DateTime
import scala.annotation.nowarn

import lishogi.common.config.Max
import lishogi.db.dsl._

final private class ChallengeRepo(coll: Coll, maxPerUser: Max)(implicit
    ec: scala.concurrent.ExecutionContext
) {

  import BSONHandlers._
  import Challenge._

  def byId(id: Challenge.ID) = coll.ext.find($id(id)).one[Challenge]

  def byIdFor(id: Challenge.ID, dest: lishogi.user.User) =
    coll.ext.find($id(id) ++ $doc("destUser.id" -> dest.id)).one[Challenge]

  def exists(id: Challenge.ID) = coll.countSel($id(id)).dmap(0 <)

  def insert(c: Challenge): Funit =
    coll.insert.one(c) >> c.challengerUser.?? { challenger =>
      createdByChallengerId(challenger.id).flatMap {
        case challenges if maxPerUser >= challenges.size => funit
        case challenges                                  => challenges.drop(maxPerUser.value).map(_.id).map(remove).sequenceFu.void
      }
    }

  def update(c: Challenge): Funit = coll.update.one($id(c.id), c).void

  def createdByChallengerId(userId: String): Fu[List[Challenge]] =
    coll.ext
      .find(selectCreated ++ $doc("challenger.id" -> userId))
      .sort($doc("createdAt" -> 1))
      .cursor[Challenge]()
      .list()

  def createdByDestId(userId: String): Fu[List[Challenge]] =
    coll.ext
      .find(selectCreated ++ $doc("destUser.id" -> userId))
      .sort($doc("createdAt" -> 1))
      .cursor[Challenge]()
      .list()

  def setChallenger(c: Challenge, color: Option[shogi.Color]) =
    coll.update
      .one(
        $id(c.id),
        $set($doc("challenger" -> c.challenger) ++ color.?? { c =>
          $doc("colorChoice" -> Challenge.ColorChoice(c), "finalColor" -> c)
        })
      )
      .void

  private[challenge] def allWithUserId(userId: String): Fu[List[Challenge]] =
    createdByChallengerId(userId) zip createdByDestId(userId) dmap { case (x, y) =>
      x ::: y
    }

  @nowarn("cat=unused") def like(c: Challenge) =
    ~(for {
      challengerId <- c.challengerUserId
      destUserId   <- c.destUserId
      if c.active
    } yield coll.one[Challenge](
      selectCreated ++ $doc(
        "challenger.id" -> challengerId,
        "destUser.id"   -> destUserId
      )
    ))

  private[challenge] def countCreatedByDestId(userId: String): Fu[Int] =
    coll.countSel(selectCreated ++ $doc("destUser.id" -> userId))

  private[challenge] def realTimeUnseenSince(date: DateTime, max: Int): Fu[List[Challenge]] = {
    val selector = $doc(
      "seenAt" $lt date,
      "status" -> Status.Created.id,
      "timeControl.l" $exists true // only realtime games
    )
    coll
      .find(selector)
      .hint(coll hint $doc("seenAt" -> 1)) // partial index
      .cursor[Challenge]()
      .list(max)
      .recoverWith { case _: reactivemongo.core.errors.DatabaseException =>
        coll.list[Challenge](selector, max)
      }
  }

  private[challenge] def expired(max: Int): Fu[List[Challenge]] =
    coll.list[Challenge]($doc("expiresAt" $lt DateTime.now), max)

  def setSeenAgain(id: Challenge.ID) =
    coll.update
      .one(
        $id(id),
        $doc(
          "$set" -> $doc(
            "status"    -> Status.Created.id,
            "seenAt"    -> DateTime.now,
            "expiresAt" -> inTwoWeeks
          )
        )
      )
      .void

  def setSeen(id: Challenge.ID) =
    coll.updateField($id(id), "seenAt", DateTime.now).void

  def offline(challenge: Challenge) = setStatus(challenge, Status.Offline, Some(_ plusHours 3))
  def cancel(challenge: Challenge)  = setStatus(challenge, Status.Canceled, Some(_ plusHours 3))
  def decline(challenge: Challenge) = setStatus(challenge, Status.Declined, Some(_ plusHours 3))
  def accept(challenge: Challenge)  = setStatus(challenge, Status.Accepted, Some(_ plusHours 3))

  def statusById(id: Challenge.ID) = coll.primitiveOne[Status]($id(id), "status")

  private def setStatus(
      challenge: Challenge,
      status: Status,
      expiresAt: Option[DateTime => DateTime]
  ) =
    coll.update
      .one(
        selectCreated ++ $id(challenge.id),
        $doc(
          "$set" -> $doc(
            "status"    -> status.id,
            "expiresAt" -> expiresAt.fold(inTwoWeeks) { _(DateTime.now) }
          )
        )
      )
      .void

  private[challenge] def remove(id: Challenge.ID) = coll.delete.one($id(id)).void

  private val selectCreated = $doc("status" -> Status.Created.id)
}
