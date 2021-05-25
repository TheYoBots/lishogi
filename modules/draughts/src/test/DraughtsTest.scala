package draughts

import draughts.format.Forsyth
import draughts.variant.Variant
import org.specs2.matcher.{ Matcher, ValidationMatchers }
import org.specs2.mutable.Specification
import scalaz.{ Validation => V }
import V.FlatMap._

trait DraughtsTest extends Specification with ValidationMatchers {

  def fenToGame(positionString: String, variant: Variant) = {
    val situation = Forsyth.<<@(variant, positionString)
    situation map { sit =>
      sit.color -> sit.board
    } toValid "Could not construct situation from FEN" map {
      case (color, board) => DraughtsGame(variant).copy(
        situation = Situation(board, color)
      )
    }
  }

  def makeBoard(pieces: (Pos, Piece)*): Board =
    Board(pieces toMap, DraughtsHistory(), draughts.variant.Standard)

  def makeBoard: Board = Board init draughts.variant.Standard

  def makeEmptyBoard: Board = Board empty draughts.variant.Standard

  def bePoss(poss: Pos*): Matcher[Option[Iterable[Pos]]] = beSome.like {
    case p => sortPoss(p.toList) must_== sortPoss(poss.toList)
  }

  def makeGame: DraughtsGame = DraughtsGame(makeBoard, White)

  def sortPoss(poss: Seq[Pos]): Seq[Pos] = poss sortBy (_.toString)
}