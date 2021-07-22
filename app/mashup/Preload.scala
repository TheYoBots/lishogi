package lishogi.app
package mashup

import lishogi.api.Context
import lishogi.event.Event
import lishogi.forum.MiniForumPost
import lishogi.game.{ Game, Pov }
import lishogi.playban.TempBan
import lishogi.simul.{ Simul, SimulIsFeaturable }
import lishogi.streamer.LiveStreams
import lishogi.timeline.Entry
import lishogi.tournament.{ Tournament, Winner }
import lishogi.tv.Tv
import lishogi.user.LightUserApi
import lishogi.user.User
import play.api.libs.json._

final class Preload(
    tv: Tv,
    gameRepo: lishogi.game.GameRepo,
    userCached: lishogi.user.Cached,
    tourWinners: lishogi.tournament.WinnersApi,
    timelineApi: lishogi.timeline.EntryApi,
    liveStreamApi: lishogi.streamer.LiveStreamApi,
    dailyPuzzle: lishogi.puzzle.DailyPuzzle.Try,
    lobbyApi: lishogi.api.LobbyApi,
    playbanApi: lishogi.playban.PlaybanApi,
    lightUserApi: LightUserApi,
    roundProxy: lishogi.round.GameProxyRepo,
    simulIsFeaturable: SimulIsFeaturable,
    lastPostCache: lishogi.blog.LastPostCache
)(implicit ec: scala.concurrent.ExecutionContext) {

  import Preload._

  def apply(
      posts: Fu[List[MiniForumPost]],
      tours: Fu[List[Tournament]],
      events: Fu[List[Event]],
      simuls: Fu[List[Simul]],
      streamerSpots: Int
  )(implicit ctx: Context): Fu[Homepage] =
    lobbyApi(ctx).mon(_.lobby segment "lobbyApi") zip
      posts.mon(_.lobby segment "posts") zip
      tours.mon(_.lobby segment "tours") zip
      events.mon(_.lobby segment "events") zip
      simuls.mon(_.lobby segment "simuls") zip
      tv.getBestGame.mon(_.lobby segment "tvBestGame") zip
      (ctx.userId ?? timelineApi.userEntriesWithLang(ctx.lang.code)).mon(_.lobby segment "timeline") zip
      userCached.topWeek.mon(_.lobby segment "userTopWeek") zip
      tourWinners.all.dmap(_.top).mon(_.lobby segment "tourWinners") zip
      (ctx.noBot ?? dailyPuzzle()).mon(_.lobby segment "puzzle") zip
      (ctx.noKid ?? liveStreamApi.all
        .dmap(_.homepage(streamerSpots, ctx.req, ctx.me) withTitles lightUserApi)
        .mon(_.lobby segment "streams")) zip
      (ctx.userId ?? playbanApi.currentBan).mon(_.lobby segment "playban") zip
      (ctx.blind ?? ctx.me ?? roundProxy.urgentGames) flatMap {
        case (
              data,
              povs
            ) ~ posts ~ tours ~ events ~ simuls ~ feat ~ entries ~ lead ~ tWinners ~ puzzle ~ streams ~ playban ~ blindGames =>
          (ctx.me ?? currentGameMyTurn(povs, lightUserApi.sync) _)
            .mon(_.lobby segment "currentGame") zip
            lightUserApi
              .preloadMany {
                tWinners.map(_.userId) ::: posts.flatMap(_.userId) ::: entries.flatMap(_.userIds).toList
              }
              .mon(_.lobby segment "lightUsers") map { case (currentGame, _) =>
              Homepage(
                data,
                entries,
                posts,
                tours,
                events,
                simuls,
                feat,
                lead,
                tWinners,
                puzzle,
                streams.excludeUsers(events.flatMap(_.hostedBy)),
                lastPostCache.apply,
                playban,
                currentGame,
                simulIsFeaturable,
                blindGames
              )
            }
      }

  def currentGameMyTurn(user: User): Fu[Option[CurrentGame]] =
    gameRepo.playingRealtimeNoAi(user).flatMap {
      _.map { roundProxy.pov(_, user) }.sequenceFu.dmap(_.flatten)
    } flatMap {
      currentGameMyTurn(_, lightUserApi.sync)(user)
    }

  private def currentGameMyTurn(povs: List[Pov], lightUser: lishogi.common.LightUser.GetterSync)(
      user: User
  ): Fu[Option[CurrentGame]] =
    ~povs.collectFirst {
      case p1 if p1.game.nonAi && p1.game.hasClock && p1.isMyTurn =>
        roundProxy.pov(p1.gameId, user) dmap (_ | p1) map { pov =>
          val opponent = lishogi.game.Namer.playerTextBlocking(pov.opponent)(lightUser)
          CurrentGame(
            pov = pov,
            opponent = opponent,
            json = Json.obj(
              "id"       -> pov.gameId,
              "color"    -> pov.color.name,
              "opponent" -> opponent
            )
          ).some
        }
    }
}

object Preload {

  case class Homepage(
      data: JsObject,
      userTimeline: Vector[Entry],
      forumRecent: List[MiniForumPost],
      tours: List[Tournament],
      events: List[Event],
      simuls: List[Simul],
      featured: Option[Game],
      leaderboard: List[User.LightPerf],
      tournamentWinners: List[Winner],
      puzzle: Option[lishogi.puzzle.DailyPuzzle.Html],
      streams: LiveStreams.WithTitles,
      lastPost: List[lishogi.blog.MiniPost],
      playban: Option[TempBan],
      currentGame: Option[Preload.CurrentGame],
      isFeaturable: Simul => Boolean,
      blindGames: List[Pov]
  )

  case class CurrentGame(pov: Pov, json: JsObject, opponent: String)
}
