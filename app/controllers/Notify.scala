package controllers

import lishogi.app._
import lishogi.notify.Notification.Notifies

import play.api.libs.json._

final class Notify(env: Env) extends lishogiController(env) {

  import env.notifyM.jsonHandlers._

  def recent(page: Int) =
    Auth { implicit ctx => me =>
      XhrOrRedirectHome {
        val notifies = Notifies(me.id)
        env.notifyM.api.getNotificationsAndCount(notifies, page) map { res =>
          Ok(Json toJson res) as JSON
        }
      }
    }
}
