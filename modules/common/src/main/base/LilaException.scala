package lishogi.base

import ornicar.scalalib.ValidTypes._

trait lishogiException extends Exception {
  val message: String

  override def getMessage = message
  override def toString   = message
}

case class lishogiInvalid(message: String) extends lishogiException

object lishogiException extends scalaz.syntax.ToShowOps {

  def apply(msg: String) =
    new lishogiException {
      val message = msg
    }

  def apply(msg: Failures): lishogiException = apply(msg.shows)
}
