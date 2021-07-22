package lishogi.puzzle

import shogi.format.{ FEN, Uci }
import reactivemongo.api.bson._
import scala.util.{ Success, Try }

import lishogi.db.BSON
import lishogi.db.dsl._
import lishogi.game.Game
import lishogi.rating.Glicko

import lishogi.game.BSONHandlers.FENBSONHandler

object BsonHandlers {

  implicit val PuzzleIdBSONHandler = stringIsoHandler(Puzzle.idIso)

  import Puzzle.{ BSONFields => F }

  implicit private[puzzle] val PuzzleBSONReader = new BSONDocumentReader[Puzzle] {
    def readDocument(r: BSONDocument) = for {
      id      <- r.getAsTry[Puzzle.Id](F.id)
      fen     <- r.getAsTry[String](F.fen)
      lineStr <- r.getAsTry[String](F.line)
      line    <- lineStr.split(' ').toList.flatMap(Uci.apply).toNel.toTry("Empty move list?!")
      glicko  <- r.getAsTry[Glicko](F.glicko)
      plays   <- r.getAsTry[Int](F.plays)
      vote    <- r.getAsTry[Float](F.vote)
      themes  <- r.getAsTry[Set[PuzzleTheme.Key]](F.themes)
      gameId      = r.getAsOpt[Game.ID](F.gameId)
      author      = r.getAsOpt[String](F.author)
      description = r.getAsOpt[String](F.description)
    } yield Puzzle(
      id = id,
      fen = fen,
      line = line,
      glicko = glicko,
      plays = plays,
      vote = vote,
      themes = themes,
      gameId = gameId,
      author = author,
      description = description
    )
  }

  implicit private[puzzle] val RoundIdHandler = tryHandler[PuzzleRound.Id](
    { case BSONString(v) =>
      v split PuzzleRound.idSep match {
        case Array(userId, puzzleId) => Success(PuzzleRound.Id(userId, Puzzle.Id(puzzleId)))
        case _                       => handlerBadValue(s"Invalid puzzle round id $v")
      }
    },
    id => BSONString(id.toString)
  )

  implicit private[puzzle] val RoundThemeHandler = tryHandler[PuzzleRound.Theme](
    { case BSONString(v) =>
      PuzzleTheme
        .find(v.tail)
        .fold[Try[PuzzleRound.Theme]](handlerBadValue(s"Invalid puzzle round theme $v")) { theme =>
          Success(PuzzleRound.Theme(theme.key, v.head == '+'))
        }
    },
    rt => BSONString(s"${if (rt.vote) "+" else "-"}${rt.theme}")
  )

  implicit private[puzzle] val RoundHandler = new BSON[PuzzleRound] {
    import PuzzleRound.BSONFields._
    def reads(r: BSON.Reader) = PuzzleRound(
      id = r.get[PuzzleRound.Id](id),
      win = r.bool(win),
      fixedAt = r.dateO(fixedAt),
      date = r.date(date),
      vote = r.intO(vote),
      themes = r.getsD[PuzzleRound.Theme](themes)
    )
    def writes(w: BSON.Writer, r: PuzzleRound) =
      $doc(
        id      -> r.id,
        win     -> r.win,
        fixedAt -> r.fixedAt,
        date    -> r.date,
        vote    -> r.vote,
        themes  -> w.listO(r.themes)
      )
  }

  implicit private[puzzle] val PathIdBSONHandler: BSONHandler[PuzzlePath.Id] = stringIsoHandler(
    PuzzlePath.pathIdIso
  )

  implicit private[puzzle] val ThemeKeyBSONHandler: BSONHandler[PuzzleTheme.Key] = stringIsoHandler(
    PuzzleTheme.keyIso
  )
}
