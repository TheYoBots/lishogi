package controllers

import org.joda.time.DateTime
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.duration._

import lila.app._

object Bot extends LilaController {

  def gameStream(id: String) = Scoped(_.Bot.Play) { req => me =>
    WithMyBotGame(id, me) { pov =>
      RequireHttp11(req) {
        lila.game.GameRepo.withInitialFen(pov.game) map { wf =>
          Ok.chunked(Env.bot.gameStateStream(wf))
        }
      }
    }
  }

  def move(id: String, uci: String) = Scoped(_.Bot.Play) { _ => me =>
    WithMyBotGame(id, me) { pov =>
      Env.bot.player(pov, uci) inject jsonOkResult recover {
        case e: Exception => BadRequest(jsonError(e.getMessage))
      }
    }
  }

  def accountUpgrade = Scoped(_.Bot.Play) { _ => me =>
    lila.user.UserRepo.setBot(me) inject jsonOkResult recover {
      case e: Exception => BadRequest(jsonError(e.getMessage))
    }
  }

  def eventStream = Scoped(_.Bot.Play) { req => me =>
    RequireHttp11(req) {
      lila.game.GameRepo.urgentGames(me) map { povs =>
        Ok.chunked(Env.bot.eventStream(me, povs.map(_.game)))
      }
    }
  }

  private def WithMyBotGame(anyId: String, me: lila.user.User)(f: lila.game.Pov => Fu[Result]) =
    lila.user.UserRepo.isBot(me) flatMap {
      case false => BadRequest(jsonError("This endpoint only works for bot accounts.")).fuccess
      case _ => Env.round.roundProxyGame(lila.game.Game takeGameId anyId) flatMap {
        case None => NotFound(jsonError("No such game")).fuccess
        case Some(game) => lila.game.Pov(game, me) match {
          case None => NotFound(jsonError("Not your game")).fuccess
          case Some(pov) => f(pov)
        }
      }
    }
}
