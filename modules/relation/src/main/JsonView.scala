package lishogi.relation

import play.api.libs.json._

object JsonView {

  implicit def relatedWrites(implicit userWrites: Writes[lishogi.user.User]) =
    OWrites[Related] { r =>
      Json.obj(
        "user"       -> r.user,
        "patron"     -> r.user.isPatron,
        "nbGames"    -> r.nbGames,
        "followable" -> r.followable,
        "relation"   -> r.relation
      )
    }
}
