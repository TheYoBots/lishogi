package lishogi.puzzle

import com.softwaremill.macwire._
import io.methvin.play.autoconfig._
import play.api.Configuration
import scala.concurrent.duration.FiniteDuration

import lishogi.common.config._
import lishogi.db.AsyncColl

@Module
private class PuzzleConfig(
    @ConfigName("mongodb.uri") val mongoUri: String,
    @ConfigName("collection.puzzle") val puzzleColl: CollName,
    @ConfigName("collection.round") val roundColl: CollName,
    @ConfigName("collection.path") val pathColl: CollName
)

case class PuzzleColls(
    puzzle: AsyncColl,
    round: AsyncColl,
    path: AsyncColl
)

@Module
final class Env(
    appConfig: Configuration,
    renderer: lishogi.hub.actors.Renderer,
    historyApi: lishogi.history.HistoryApi,
    lightUserApi: lishogi.user.LightUserApi,
    cacheApi: lishogi.memo.CacheApi,
    gameRepo: lishogi.game.GameRepo,
    userRepo: lishogi.user.UserRepo,
    mongo: lishogi.db.Env
)(implicit
    ec: scala.concurrent.ExecutionContext,
    system: akka.actor.ActorSystem,
    mode: play.api.Mode
) {

  private val config = appConfig.get[PuzzleConfig]("puzzle")(AutoConfig.loader)

  private lazy val db = mongo.asyncDb("puzzle", config.mongoUri)

  lazy val colls = PuzzleColls(
    puzzle = db(config.puzzleColl),
    round = db(config.roundColl),
    path = db(config.pathColl)
  )

  private lazy val gameJson: GameJson = wire[GameJson]

  lazy val jsonView = wire[JsonView]

  private lazy val pathApi = wire[PuzzlePathApi]

  private lazy val trustApi = wire[PuzzleTrustApi]

  private lazy val countApi = wire[PuzzleCountApi]

  lazy val api: PuzzleApi = wire[PuzzleApi]

  lazy val session: PuzzleSessionApi = wire[PuzzleSessionApi]

  lazy val anon: PuzzleAnon = wire[PuzzleAnon]

  lazy val batch: PuzzleBatch = wire[PuzzleBatch]

  lazy val finisher = wire[PuzzleFinisher]

  lazy val forms = PuzzleForm

  lazy val daily = wire[DailyPuzzle]

  lazy val activity = wire[PuzzleActivity]

  lazy val dashboard = wire[PuzzleDashboardApi]

  lazy val replay = wire[PuzzleReplayApi]

  lazy val history = wire[PuzzleHistoryApi]

  def cli =
    new lishogi.common.Cli {
      def process = { case "puzzle" :: "delete" :: id :: Nil =>
        api.puzzle delete Puzzle.Id(id) inject "Done"
      }
    }
}
