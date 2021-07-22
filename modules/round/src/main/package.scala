package lishogi

import lishogi.game.Event

package object round extends PackageObject {

  private[round] type Events = List[Event]

  private[round] def logger = lishogi.log("round")
}

package round {

  trait BenignError                        extends lishogi.base.lishogiException
  case class ClientError(message: String)  extends BenignError
  case class FishnetError(message: String) extends BenignError

  sealed trait OnTv

  case class OnLishogiTv(channel: String, flip: Boolean) extends OnTv
  case class OnUserTv(userId: String)                    extends OnTv
}
