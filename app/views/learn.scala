package views.html.learn

import play.api.libs.json.Json

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.common.String.html.safeJsonValue

import controllers.routes

object index {

  import trans.learn.{ play => _, _ }

  def apply(data: Option[play.api.libs.json.JsValue])(implicit ctx: Context) =
    views.html.base.layout(
      title = s"${learnShogi.txt()} - ${byPlaying.txt()}",
      moreJs = frag(
        jsAt(s"compiled/lishogi.learn${isProd ?? ".min"}.js"),
        embedJsUnsafe(s"""$$(function() {
LishogiLearn(document.getElementById('learn-app'), ${safeJsonValue(
          Json.obj(
            "data" -> data,
            "i18n" -> i18nJsObject(i18nKeys)
          )
        )})})""")
      ),
      moreCss = cssTag("learn"),
      openGraph = lila.app.ui
        .OpenGraph(
          title = "Learn shogi by playing",
          description = "You don't know much about shogi? Excellent! Let's have fun and learn to play shogi!",
          url = s"$netBaseUrl${routes.Learn.index()}"
        )
        .some,
      zoomable = true
    ) {
      main(id := "learn-app")
    }

  private val i18nKeys: List[lila.i18n.MessageKey] =
    List(
      learnShogi,
      byPlaying,
      menu,
      progressX,
      resetMyProgress,
      youWillLoseAllYourProgress,
      trans.learn.play,
      shogiPieces,
      theIntoduction,
      introIntro,
      introBasics,
      theIntoduction,
      choosePieceDesign,
      promotionZone,
      introComplete,
      theRook,
      itMovesInStraightLines,
      rookIntro,
      rookGoal,
      rookPromotion,
      rookSummary,
      dragonSummary,
      grabAllTheStars,
      theFewerMoves,
      rookComplete,
      theBishop,
      itMovesDiagonally,
      bishopIntro,
      bishopPromotion,
      bishopSummary,
      horseSummary,
      bishopComplete,
      theKing,
      theMostImportantPiece,
      kingIntro,
      theKingIsSlow,
      kingSummary,
      kingComplete,
      theKnight,
      itMovesInAnLShape,
      knightIntro,
      knightsHaveAFancyWay,
      knightsCanJumpOverObstacles,
      knightSummary,
      pknightSummary,
      knightPromotion,
      knightComplete,
      thePawn,
      itMovesForwardOnly,
      pawnIntro,
      pawnsMoveOneSquareOnly,
      pawnPromotion,
      pawnSummary,
      tokinSummary,
      pawnComplete,
      theLance,
      itMovesStraightForward,
      lanceIntro,
      lancesAreStraighforward,
      lancePromotion,
      lanceSummary,
      planceSummary,
      lanceComplete,
      theGold,
      itMovesInAnyDirectionExceptDiagonallyBack,
      goldIntro,
      goldDoesntPromote,
      goldSummary,
      goldComplete,
      theSilver,
      itMovesEitherForwardOrDiagonallyBack,
      silverIntro,
      silverPromotion,
      silverSummary,
      psilverSummary,
      silverComplete,
      itNowPromotesToAStrongerPiece,
      selectThePieceYouWant,
      fundamentals,
      capture,
      takeTheEnemyPieces,
      captureIntro,
      takeTheBlackPieces,
      takeTheBlackPiecesAndDontLoseYours,
      captureComplete,
      protection,
      keepYourPiecesSafe,
      protectionIntro,
      protectionComplete,
      escape,
      noEscape,
      dontLetThemTakeAnyUndefendedPiece,
      combat,
      captureAndDefendPieces,
      combatIntro,
      combatComplete,
      checkInOne,
      attackTheOpponentsKing,
      checkInOneIntro,
      checkInOneGoal,
      checkInOneComplete,
      outOfCheck,
      defendYourKing,
      outOfCheckIntro,
      escapeWithTheKing,
      theKingCannotEscapeButBlock,
      youCanGetOutOfCheckByTaking,
      thisKnightIsCheckingThroughYourDefenses,
      escapeOrBlock,
      outOfCheckComplete,
      mateInOne,
      defeatTheOpponentsKing,
      mateInOneIntro,
      attackYourOpponentsKing,
      mateInOneComplete,
      intermediate,
      boardSetup,
      howTheGameStarts,
      boardSetupIntro,
      thisIsTheInitialPosition,
      firstPlaceTheRooks,
      thenPlaceTheKnights,
      placeTheBishops,
      placeTheQueen,
      placeTheKing,
      pawnsFormTheFrontLine,
      boardSetupComplete,
      stalemate,
      theGameIsADraw,
      stalemateIntro,
      stalemateGoal,
      stalemateComplete,
      advanced,
      pieceValue,
      evaluatePieceStrength,
      pieceValueIntro,
      queenOverBishop,
      takeThePieceWithTheHighestValue,
      pieceValueComplete,
      checkInTwo,
      twoMovesToGiveCheck,
      checkInTwoIntro,
      checkInTwoGoal,
      checkInTwoComplete,
      whatNext,
      youKnowHowToPlayChess,
      register,
      getAFreeLishogiAccount,
      practice,
      learnCommonChessPositions,
      puzzles,
      exerciseYourTacticalSkills,
      videos,
      watchInstructiveChessVideos,
      playPeople,
      opponentsFromAroundTheWorld,
      playMachine,
      testYourSkillsWithTheComputer,
      letsGo,
      stageX,
      awesome,
      excellent,
      greatJob,
      perfect,
      outstanding,
      wayToGo,
      yesYesYes,
      youreGoodAtThis,
      nailedIt,
      rightOn,
      stageXComplete,
      yourScore,
      next,
      backToMenu,
      puzzleFailed,
      retry
    ).map(_.key)
}
