package lidraughts.importer

import draughts.format.pdn.{ Parser, Reader, ParsedPdn, Tag, TagType, Tags }
import draughts.format.{ FEN, Forsyth }
import draughts.{ Replay, Color, Mode, Status }
import play.api.data._
import play.api.data.Forms._
import scalaz.Validation.FlatMap._

import lidraughts.game._

private[importer] final class DataForm {

  lazy val importForm = Form(mapping(
    "pdn" -> nonEmptyText.verifying("invalidPdn", p => checkPdn(p).isSuccess),
    "analyse" -> optional(nonEmptyText)
  )(ImportData.apply)(ImportData.unapply))

  def checkPdn(pdn: String): Valid[Preprocessed] = ImportData(pdn, none).preprocess(none)
}

private[importer] case class TagResult(status: Status, winner: Option[Color])
case class Preprocessed(
    game: NewGame,
    replay: Replay,
    initialFen: Option[FEN],
    parsed: ParsedPdn
)

case class ImportData(pdn: String, analyse: Option[String]) {

  private type TagPicker = Tag.type => TagType

  private val maxPlies = 500

  private def evenIncomplete(result: Reader.Result): Replay = result match {
    case Reader.Result.Complete(replay) => replay
    case Reader.Result.Incomplete(replay, _) => replay
  }

  def preprocess(user: Option[String]): Valid[Preprocessed] =
    Parser.full(pdn) map { parsed =>
      val replay = Reader.fullWithSans(
        parsed,
        sans => sans.copy(value = sans.value take maxPlies),
        iteratedCapts = true
      ) |> evenIncomplete

      val initialFen = Forsyth.>>(replay.setup)
      val fromPosition = initialFen != Forsyth.initial && initialFen != Forsyth.initialMoveAndPieces
      val variant = {
        parsed.tags.variant | {
          if (fromPosition) draughts.variant.FromPosition
          else draughts.variant.Standard
        }
      } match {
        case draughts.variant.FromPosition if !fromPosition => draughts.variant.Standard
        case draughts.variant.Standard if fromPosition => draughts.variant.FromPosition
        case v => v
      }

      val game = replay.state.copy(situation = replay.state.situation withVariant variant)

      val status = parsed.tags(_.Termination).map(_.toLowerCase) match {
        case Some("normal") | None => Status.Resign
        case Some("abandoned") => Status.Aborted
        case Some("time forfeit") => Status.Outoftime
        case Some("rules infraction") => Status.Cheat
        case Some(_) => Status.UnknownFinish
      }

      val date = parsed.tags.anyDate

      def name(whichName: TagPicker, whichRating: TagPicker): String = parsed.tags(whichName).fold("?") { n =>
        n + ~parsed.tags(whichRating).map(e => s" (${e take 8})")
      }

      val dbGame = Game.make(
        draughts = game,
        whitePlayer = Player.make(draughts.White, None) withName name(_.White, _.WhiteElo),
        blackPlayer = Player.make(draughts.Black, None) withName name(_.Black, _.BlackElo),
        mode = Mode.Casual,
        source = Source.Import,
        pdnImport = PdnImport.make(user = user, date = date, pdn = pdn).some
      ).sloppy.start |> { dbGame =>
        // apply the result from the board or the tags
        game.situation.status match {
          case Some(situationStatus) => dbGame.finish(situationStatus, game.situation.winner).game
          case None => parsed.tags.resultColor.map {
            case Some(color) => TagResult(status, color.some)
            case None if status == Status.Outoftime => TagResult(status, none)
            case None => TagResult(Status.Draw, none)
          }.filter(_.status > Status.Started).fold(dbGame) { res =>
            dbGame.finish(res.status, res.winner).game
          }
        }
      }

      val dropMoves = parsed.sans.value.length - replay.moves.length
      Preprocessed(
        NewGame(dbGame),
        replay.copy(state = game),
        fromPosition option FEN(initialFen),
        parsed.copy(sans = draughts.format.pdn.Sans(parsed.sans.value.drop(dropMoves)))
      )
    }
}
