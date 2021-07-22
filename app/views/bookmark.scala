package views.html

import lishogi.api.Context
import lishogi.app.templating.Environment._
import lishogi.app.ui.ScalatagsTemplate._

import controllers.routes

object bookmark {

  def toggle(g: lishogi.game.Game, bookmarked: Boolean)(implicit ctx: Context) =
    if (ctx.isAuth)
      a(
        cls := List(
          "bookmark"   -> true,
          "bookmarked" -> bookmarked
        ),
        href := routes.Bookmark.toggle(g.id),
        title := trans.bookmarkThisGame.txt()
      )(
        iconTag("t")(cls := "on is3"),
        iconTag("s")(cls := "off is3"),
        span(g.showBookmarks)
      )
    else if (g.hasBookmarks)
      span(cls := "bookmark")(
        span(dataIcon := "s", cls := "is3")(g.showBookmarks)
      )
    else emptyFrag
}
