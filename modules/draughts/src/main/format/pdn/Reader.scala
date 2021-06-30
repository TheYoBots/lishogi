package draughts
package format.pdn

import scalaz.Validation.{ success, failure }

object Reader {

  sealed trait Result {
    def valid: Valid[Replay]
  }

  object Result {
    case class Complete(replay: Replay) extends Result {
      def valid = success(replay)
    }
    case class Incomplete(replay: Replay, failures: Failures) extends Result {
      def valid = failure(failures)
    }
  }

  def moves(moveStrs: Traversable[String], tags: Tags, iteratedCapts: Boolean = false): Valid[Result] =
    movesWithSans(moveStrs, identity, tags, iteratedCapts)

  def fullWithSans(parsed: ParsedPdn, op: Sans => Sans, iteratedCapts: Boolean = false): Result =
    makeReplay(makeGame(parsed.tags, parsed.initialPosition.some), op(parsed.sans), iteratedCapts)

  def movesWithSans(moveStrs: Traversable[String], op: Sans => Sans, tags: Tags, iteratedCapts: Boolean = false): Valid[Result] =
    Parser.moves(moveStrs, tags.variant | variant.Variant.default) map { moves =>
      makeReplay(makeGame(tags), op(moves), iteratedCapts)
    }

  private def makeReplay(game: DraughtsGame, sans: Sans, iteratedCapts: Boolean = false): Result = {
    def mk(replay: Replay, moves: List[San], ambs: List[(San, String)]): Result = {
      var newAmb = none[(San, String)]
      val res = moves match {
        case san :: rest =>
          val ambiguities = if (ambs.isEmpty) none else ambs.collect({ case (ambSan, ambUci) if ambSan == san => ambUci }).some
          san(replay.state.situation, iteratedCapts, ambiguities).fold(
            err => Result.Incomplete(replay, err),
            move => {
              if (iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore.ambiguitiesMove(move) > ambs.length + 1)
                newAmb = (san -> move.toUci.uci).some
              val fenComment = san.metas.fenComment(game.board.variant)
              if (fenComment.isDefined && rest.nonEmpty) {
                // restart at each fen comment
                val g = DraughtsGame(
                  variantOption = game.board.variant.some,
                  fen = fenComment
                )
                val newReplay = Replay(g.copy(
                  startedAtTurn = replay.state.turns,
                  clock = game.clock
                ))
                newAmb = none
                mk(newReplay, rest, Nil)
              } else mk(replay.addMove(move, iteratedCapts), rest, newAmb.fold(ambs)(_ :: ambs))
            }
          )
        case _ => Result.Complete(replay)
      }
      res match {
        case Result.Incomplete(_, _) if newAmb.isDefined => mk(replay, moves, newAmb.get :: ambs)
        case _ => res
      }
    }
    mk(Replay(game), sans.value, Nil)
  }

  private def makeGame(tags: Tags, maybeInit: Option[InitialPosition] = None): DraughtsGame = {
    val fenComment = maybeInit match {
      case Some(init) =>
        init.initialFen(tags.variant | variant.Standard)
      case _ => none
    }
    val g = DraughtsGame(
      variantOption = tags.variant,
      fen = fenComment orElse tags(_.FEN)
    )
    g.copy(
      startedAtTurn = g.turns,
      clock = tags.clockConfig map Clock.apply
    )
  }
}
