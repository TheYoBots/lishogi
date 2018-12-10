package lidraughts.socket

import ornicar.scalalib.Random.approximatly
import play.api.libs.json._
import scala.concurrent.duration._
import scala.concurrent.Promise

import actorApi._
import draughts.Centis
import lidraughts.common.LightUser
import lidraughts.hub.actorApi.Deploy
import lidraughts.hub.actorApi.socket.HasUserId
import lidraughts.hub.Trouper
import lidraughts.memo.ExpireSetMemo

abstract class SocketTrouper[M <: SocketMember](
    protected val system: akka.actor.ActorSystem,
    protected val uidTtl: Duration
) extends Socket with Trouper {

  import SocketTrouper._

  override def stop() = {
    super.stop()
    members foreachKey ejectUidString
  }

  protected val receiveTrouper: PartialFunction[Any, Unit] = {

    case HasUserId(userId, promise) => promise success hasUserId(userId)

    case GetNbMembers(promise) => promise success members.size
  }

  val process = receiveSpecific orElse receiveTrouper orElse receiveGeneric

  protected val members = scala.collection.mutable.AnyRefMap.empty[String, M]
  protected val aliveUids = new ExpireSetMemo(uidTtl)
  protected var pong = Socket.initialPong

  protected def lidraughtsBus = system.lidraughtsBus

  // to be defined in subclassing socket
  protected def receiveSpecific: PartialFunction[Any, Unit]

  // generic message handler
  protected def receiveGeneric: PartialFunction[Any, Unit] = {

    case Ping(uid, _, lagCentis) => ping(uid, lagCentis)

    case Broom => broom

    // when a member quits
    case Quit(uid) => quit(uid)

    case Resync(uid) => resync(uid)

    case d: Deploy => onDeploy(d)
  }

  protected def hasUserId(userId: String) = members.values.exists(_.userId contains userId)

  protected def notifyAll[A: Writes](t: String, data: A): Unit =
    notifyAll(makeMessage(t, data))

  protected def notifyAll(t: String): Unit =
    notifyAll(makeMessage(t))

  protected def notifyAll(msg: JsObject): Unit =
    members.foreachValue(_ push msg)

  protected def notifyIf(msg: JsObject)(condition: M => Boolean): Unit =
    members.foreachValue { member =>
      if (condition(member)) member push msg
    }

  protected def notifyMember[A: Writes](t: String, data: A)(member: M): Unit = {
    member push makeMessage(t, data)
  }

  protected def notifyUid[A: Writes](t: String, data: A)(uid: Socket.Uid): Unit = {
    withMember(uid)(_ push makeMessage(t, data))
  }

  protected def ping(uid: Socket.Uid, lagCentis: Option[Centis]): Unit = {
    setAlive(uid)
    withMember(uid) { member =>
      member push pong
      for {
        lc <- lagCentis
        user <- member.userId
      } UserLagCache.put(user, lc)
    }
  }

  protected def broom: Unit =
    members.keys foreach { uid =>
      if (!aliveUids.get(uid)) ejectUidString(uid)
    }

  protected def ejectUidString(uid: String): Unit = eject(Socket.Uid(uid))

  protected def eject(uid: Socket.Uid): Unit = withMember(uid) { member =>
    member.end
    quit(uid)
  }

  protected def quit(uid: Socket.Uid): Unit = withMember(uid) { member =>
    members -= uid.value
    lidraughtsBus.publish(SocketLeave(uid, member), 'socketLeave)
  }

  protected def onDeploy(d: Deploy): Unit =
    notifyAll(makeMessage(d.key))

  protected val resyncMessage = makeMessage("resync")

  protected def resync(member: M): Unit = {
    import scala.concurrent.duration._
    system.scheduler.scheduleOnce((scala.util.Random nextInt 2000).milliseconds) {
      resyncNow(member)
    }
  }

  protected def resync(uid: Socket.Uid): Unit =
    withMember(uid)(resync)

  protected def resyncNow(member: M): Unit =
    member push resyncMessage

  protected def addMember(uid: Socket.Uid, member: M): Unit = {
    eject(uid)
    members += (uid.value -> member)
    setAlive(uid)
    lidraughtsBus.publish(SocketEnter(uid, member), 'socketEnter)
  }

  protected def setAlive(uid: Socket.Uid): Unit = aliveUids put uid.value

  protected def membersByUserId(userId: String): Iterable[M] = members collect {
    case (_, member) if member.userId.contains(userId) => member
  }

  protected def firstMemberByUserId(userId: String): Option[M] = members collectFirst {
    case (_, member) if member.userId.contains(userId) => member
  }

  protected def uidToUserId(uid: Socket.Uid): Option[String] = members get uid.value flatMap (_.userId)

  protected val maxSpectatorUsers = 15

  protected def showSpectators(lightUser: LightUser.Getter)(watchers: Iterable[SocketMember]): Fu[JsValue] = watchers.size match {
    case 0 => fuccess(JsNull)
    case s if s > maxSpectatorUsers => fuccess(Json.obj("nb" -> s))
    case s => {
      val userIdsWithDups = watchers.toSeq.flatMap(_.userId)
      val anons = s - userIdsWithDups.size
      val userIds = userIdsWithDups.distinct

      val total = anons + userIds.size

      userIds.map(lightUser).sequenceFu.map { users =>
        Json.obj(
          "nb" -> total,
          "users" -> users.flatten.map(_.titleName),
          "anons" -> anons
        )
      }
    }
  }

  protected def withMember(uid: Socket.Uid)(f: M => Unit): Unit = members get uid.value foreach f
}

object SocketTrouper {
  case class GetNbMembers(promise: Promise[Int])
}

// Not managed by a TrouperMap
trait LoneSocket { self: SocketTrouper[_] =>

  def monitoringName: String
  def broomFrequency: FiniteDuration

  system.scheduler.schedule(approximatly(0.1f)(12.seconds.toMillis).millis, broomFrequency) {
    this ! lidraughts.socket.actorApi.Broom
    lidraughts.mon.socket.queueSize(monitoringName)(queueSize)
  }
  system.lidraughtsBus.subscribe(this, 'deploy, 'shutdown)
}
