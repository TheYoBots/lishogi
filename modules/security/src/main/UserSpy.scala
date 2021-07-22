package lishogi.security

import org.joda.time.DateTime
import reactivemongo.api.ReadPreference
import reactivemongo.api.bson._
import org.uaparser.scala.{ Parser => UAParser }
import org.uaparser.scala.Client

import lishogi.common.{ EmailAddress, IpAddress }
import lishogi.db.dsl._
import lishogi.user.{ User, UserRepo }

case class UserSpy(
    ips: List[UserSpy.IPData],
    prints: List[UserSpy.FPData],
    uas: List[Dated[UserSpy.UaAndClient]],
    otherUsers: List[UserSpy.OtherUser]
) {

  import UserSpy.OtherUser

  def rawIps = ips map (_.ip.value)
  def rawFps = prints map (_.fp.value)

  def otherUserIds = otherUsers.map(_.user.id)

  def usersSharingIp =
    otherUsers.collect {
      case OtherUser(user, ips, _) if ips.nonEmpty => user
    }

  def distinctLocations = UserSpy.distinctRecent(ips.map(_.datedLocation).sortBy(-_.seconds))
}

final class UserSpyApi(
    firewall: Firewall,
    store: Store,
    userRepo: UserRepo,
    geoIP: GeoIP,
    ip2proxy: Ip2Proxy,
    printBan: PrintBan
)(implicit ec: scala.concurrent.ExecutionContext) {

  import UserSpy._

  def apply(user: User, maxOthers: Int): Fu[UserSpy] =
    store.chronoInfoByUser(user) flatMap { infos =>
      val ips = distinctRecent(infos.map(_.datedIp))
      val fps = distinctRecent(infos.flatMap(_.datedFp))
      fetchOtherUsers(user, ips.map(_.value).toSet, fps.map(_.value).toSet, maxOthers) zip
        ip2proxy.keepProxies(ips.map(_.value).toList) map { case (otherUsers, proxies) =>
          val othersByIp = otherUsers.foldLeft(Map.empty[IpAddress, Set[User]]) { case (acc, other) =>
            other.ips.foldLeft(acc) { case (acc, ip) =>
              acc.updated(ip, acc.getOrElse(ip, Set.empty) + other.user)
            }
          }
          val othersByFp = otherUsers.foldLeft(Map.empty[FingerHash, Set[User]]) { case (acc, other) =>
            other.fps.foldLeft(acc) { case (acc, fp) =>
              acc.updated(fp, acc.getOrElse(fp, Set.empty) + other.user)
            }
          }
          UserSpy(
            ips = ips.map { ip =>
              IPData(
                ip,
                firewall blocksIp ip.value,
                geoIP orUnknown ip.value,
                proxies(ip.value),
                Alts(othersByIp.getOrElse(ip.value, Set.empty))
              )
            }.toList,
            prints = fps.map { fp =>
              FPData(
                fp,
                printBan blocks fp.value,
                Alts(othersByFp.getOrElse(fp.value, Set.empty))
              )
            }.toList,
            uas = distinctRecent(infos.map(_.datedUa map { ua =>
              UaAndClient(ua, parseUa(ua))
            })).toList,
            otherUsers = otherUsers
          )
        }
    }

  private[security] def userHasPrint(u: User): Fu[Boolean] =
    store.coll.secondaryPreferred.exists($doc("user" -> u.id, "fp" $exists true))

  private val parseUa = UAParser.default.parse _

  private def fetchOtherUsers(
      user: User,
      ipSet: Set[IpAddress],
      fpSet: Set[FingerHash],
      max: Int
  ): Fu[List[OtherUser]] =
    ipSet.nonEmpty ?? store.coll
      .aggregateList(max, readPreference = ReadPreference.secondaryPreferred) { implicit framework =>
        import framework._
        import FingerHash.fpHandler
        Match(
          $doc(
            $or(
              "ip" $in ipSet,
              "fp" $in fpSet
            ),
            "user" $ne user.id,
            "date" $gt DateTime.now.minusYears(1)
          )
        ) -> List(
          GroupField("user")(
            "ips" -> AddFieldToSet("ip"),
            "fps" -> AddFieldToSet("fp")
          ),
          AddFields(
            $doc(
              "nbIps" -> $doc("$size" -> "$ips"),
              "nbFps" -> $doc("$size" -> "$fps")
            )
          ),
          AddFields(
            $doc(
              "score" -> $doc(
                "$add" -> $arr("$nbIps", "$nbFps", $doc("$multiply" -> $arr("$nbIps", "$nbFps")))
              )
            )
          ),
          Sort(Descending("score")),
          Limit(max),
          PipelineOperator(
            $doc(
              "$lookup" -> $doc(
                "from"         -> userRepo.coll.name,
                "localField"   -> "_id",
                "foreignField" -> "_id",
                "as"           -> "user"
              )
            )
          ),
          UnwindField("user")
        )
      }
      .map { docs =>
        import lishogi.user.User.userBSONHandler
        for {
          doc  <- docs
          user <- doc.getAsOpt[User]("user")
          ips  <- doc.getAsOpt[Set[IpAddress]]("ips")
          fps  <- doc.getAsOpt[Set[FingerHash]]("fps")
        } yield OtherUser(user, ips intersect ipSet, fps intersect fpSet)
      }

  def getUserIdsWithSameIpAndPrint(userId: User.ID): Fu[Set[User.ID]] =
    for {
      (ips, fps) <- nextValues("ip", userId) zip nextValues("fp", userId)
      users <- (ips.nonEmpty && fps.nonEmpty) ?? store.coll.secondaryPreferred.distinctEasy[User.ID, Set](
        "user",
        $doc(
          "ip" $in ips,
          "fp" $in fps,
          "user" $ne userId
        )
      )
    } yield users

  private def nextValues(field: String, userId: User.ID): Fu[Set[String]] =
    store.coll.secondaryPreferred.distinctEasy[String, Set](field, $doc("user" -> userId))
}

object UserSpy {

  case class OtherUser(user: User, ips: Set[IpAddress], fps: Set[FingerHash]) {
    val nbIps = ips.size
    val nbFps = fps.size
    val score = nbIps + nbFps + nbIps * nbFps
  }

  // distinct values, keeping the most recent of duplicated values
  // assumes all is sorted by most recent first
  def distinctRecent[V](all: List[Dated[V]]): scala.collection.View[Dated[V]] =
    all
      .foldLeft(Map.empty[V, DateTime]) {
        case (acc, Dated(v, _)) if acc.contains(v) => acc
        case (acc, Dated(v, date))                 => acc + (v -> date)
      }
      .view
      .map { case (v, date) => Dated(v, date) }

  case class Alts(users: Set[User]) {
    lazy val boosters = users.count(_.marks.boost)
    lazy val engines  = users.count(_.marks.engine)
    lazy val trolls   = users.count(_.marks.troll)
    lazy val alts     = users.count(_.marks.alt)
    lazy val closed   = users.count(u => u.disabled && u.marks.clean)
    lazy val cleans   = users.count(u => u.enabled && u.marks.clean)
    def score =
      (boosters * 10 + engines * 10 + trolls * 10 + alts * 10 + closed * 2 + cleans) match {
        case 0 => -999999 // rank empty alts last
        case n => n
      }
  }

  case class IPData(
      ip: Dated[IpAddress],
      blocked: Boolean,
      location: Location,
      proxy: Boolean,
      alts: Alts
  ) {
    def datedLocation = Dated(location, ip.date)
  }

  case class FPData(
      fp: Dated[FingerHash],
      banned: Boolean,
      alts: Alts
  )

  case class UaAndClient(ua: String, client: Client) {
    def mobile = ua contains "Mobile"
    def app    = ua contains "lichobile"
  }

  case class WithMeSortedWithEmails(others: List[OtherUser], emails: Map[User.ID, EmailAddress]) {
    def emailValueOf(u: User) = emails.get(u.id).map(_.value)
    def size                  = others.size
  }

  def withMeSortedWithEmails(
      userRepo: UserRepo,
      me: User,
      spy: UserSpy
  )(implicit ec: scala.concurrent.ExecutionContext): Fu[WithMeSortedWithEmails] =
    userRepo.emailMap(me.id :: spy.otherUsers.map(_.user.id)) map { emailMap =>
      WithMeSortedWithEmails(
        (OtherUser(me, spy.rawIps.toSet, spy.rawFps.toSet) :: spy.otherUsers),
        emailMap
      )
    }
}
