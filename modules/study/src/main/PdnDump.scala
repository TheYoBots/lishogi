package lidraughts.study

import draughts.format.Forsyth
import draughts.format.pdn.{ Pdn, Tag, Tags, Initial }
import draughts.format.{ pdn => draughtsPdn, FEN }
import draughts.{ Centis, Color }
import draughts.variant.Variant
import org.joda.time.format.DateTimeFormat

import lidraughts.common.LightUser
import lidraughts.common.String.slugify
import lidraughts.tree.Node.{ Shape, Shapes, Comment }

final class PdnDump(
    chapterRepo: ChapterRepo,
    gamePdnDump: lidraughts.game.PdnDump,
    lightUser: LightUser.GetterSync,
    netBaseUrl: String
) {

  import PdnDump._

  def apply(study: Study): Fu[List[Pdn]] =
    chapterRepo.orderedByStudy(study.id).map {
      _.map { ofChapter(study, _) }
    }

  def ofChapter(study: Study, chapter: Chapter) = Pdn(
    tags = makeTags(study, chapter),
    turns = toTurns(chapter.root, chapter.setup.variant),
    initial = Initial(
      chapter.root.comments.list.map(_.text.value) ::: shapeComment(chapter.root.shapes).toList
    )
  )

  private val fileR = """[\s,]""".r

  def ownerName(study: Study) = lightUser(study.ownerId).fold(study.ownerId)(_.name)

  def filename(study: Study): String = {
    val date = dateFormat.print(study.createdAt)
    fileR.replaceAllIn(
      s"lidraughts_study_${slugify(study.name.value)}_by_${ownerName(study)}_${date}.pdn", ""
    )
  }

  def filename(study: Study, chapter: Chapter): String = {
    val date = dateFormat.print(chapter.createdAt)
    fileR.replaceAllIn(
      s"lidraughts_study_${slugify(study.name.value)}_${slugify(chapter.name.value)}_by_${ownerName(study)}_${date}.pdn", ""
    )
  }

  private def chapterUrl(studyId: Study.Id, chapterId: Chapter.Id) = s"$netBaseUrl/study/$studyId/$chapterId"

  private val dateFormat = DateTimeFormat forPattern "yyyy.MM.dd";

  private def annotatorTag(study: Study) =
    Tag(_.Annotator, s"https://lidraughts.org/@/${ownerName(study)}")

  private def makeTags(study: Study, chapter: Chapter): Tags = Tags {
    val opening = chapter.opening
    val genTags = List(
      Tag(_.Event, s"${study.name}: ${chapter.name}"),
      Tag(_.Site, chapterUrl(study.id, chapter.id)),
      Tag(_.UTCDate, Tag.UTCDate.format.print(chapter.createdAt)),
      Tag(_.UTCTime, Tag.UTCTime.format.print(chapter.createdAt)),
      Tag(_.GameType, chapter.setup.variant.gameType),
      Tag(_.Opening, opening.fold("?")(_.name)),
      Tag(_.Result, "*") // required for SCID to import
    ) ::: List(annotatorTag(study)) ::: (chapter.root.fen.value != Forsyth.initial).??(List(
        Tag(_.FEN, Forsyth.shorten(chapter.root.fen.value))
      //Tag("SetUp", "1")
      ))
    genTags.foldLeft(chapter.tags.value.reverse) {
      case (tags, tag) =>
        if (tags.exists(t => tag.name == t.name)) tags
        else tag :: tags
    }.reverse
  }
}

private[study] object PdnDump {

  private type Variations = Vector[Node]
  private val noVariations: Variations = Vector.empty

  def node2move(node: Node, variations: Variations, turn: Color, whiteClock: Option[Centis], blackClock: Option[Centis], fenBefore: FEN, variant: Variant) = {
    val uci = node.move.uci
    val pdnMove =
      if (node.move.san.indexOf('x') == -1) node.move.san
      else {
        val situation = Forsyth.<<@(variant, fenBefore.value)
        if (situation.fold(false)(_.ambiguitiesMove(uci.origDest._1, uci.origDest._2) > 1)) uci.toFullSan
        else node.move.san
      }
    draughtsPdn.Move(
      san = pdnMove,
      turn = turn,
      glyphs = node.glyphs,
      comments = node.comments.list.map(_.text.value) ::: shapeComment(node.shapes).toList,
      opening = none,
      result = none,
      variations = variations.map { child =>
        toTurns(child.mainline, noVariations, fenBefore, variant)
      }(scala.collection.breakOut),
      secondsLeft = (whiteClock.map(_.roundSeconds), blackClock.map(_.roundSeconds))
    )
  }

  // [%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]
  private def shapeComment(shapes: Shapes): Option[String] = {
    def render(as: String)(shapes: List[String]) = shapes match {
      case Nil => ""
      case shapes => s"[%$as ${shapes.mkString(",")}]"
    }
    val circles = render("csl") {
      shapes.value.collect {
        case Shape.Circle(brush, orig) => s"${brush.head.toUpper}$orig"
      }
    }
    val arrows = render("cal") {
      shapes.value.collect {
        case Shape.Arrow(brush, orig, dest) => s"${brush.head.toUpper}$orig$dest"
      }
    }
    s"$circles$arrows".some.filter(_.nonEmpty)
  }

  def toTurn(first: Node, second: Option[Node], variations: Variations, fenBefore: FEN, variant: Variant) = draughtsPdn.Turn(
    number = first.fullMoveNumber,
    white = node2move(first, variations, Color.White, first.clock, second.flatMap(_.clock), fenBefore, variant).some,
    black = second map { s => node2move(s, first.children.variations, Color.Black, first.clock, s.clock, first.fen, variant) }
  )

  def toTurns(root: Node.Root, variant: Variant): List[draughtsPdn.Turn] = toTurns(root.mainline, root.children.variations, root.fen, variant)

  def toTurns(line: List[Node], variations: Variations, fenBefore: FEN, variant: Variant): List[draughtsPdn.Turn] = (line match {
    case Nil => Nil
    case first :: rest if first.ply % 2 == 0 => draughtsPdn.Turn(
      number = 1 + (first.ply - 1) / 2,
      white = none,
      black = node2move(first, variations, Color.Black, rest.headOption.flatMap(_.clock), first.clock, fenBefore, variant).some
    ) :: toTurnsFromWhite(rest, first.children.variations, first.fen, variant)
    case l => toTurnsFromWhite(l, variations, fenBefore, variant)
  }) filterNot (_.isEmpty)

  def toTurnsFromWhite(line: List[Node], variations: Variations, fenBefore: FEN, variant: Variant): List[draughtsPdn.Turn] =
    (line grouped 2).foldLeft((variations, List.empty[draughtsPdn.Turn], fenBefore)) {
      case ((variations, turns, fen), pair) => pair.headOption.fold((variations, turns, fen)) { first =>
        val second = pair lift 1
        val nextFen = second.map(_.fen).getOrElse(first.fen)
        (pair.lift(1).getOrElse(first).children.variations, toTurn(first, second, variations, fen, variant) :: turns, nextFen)
      }
    }._2.reverse
}
