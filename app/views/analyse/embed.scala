package views.html.analyse

import play.api.libs.json.Json

import lidraughts.api.Context
import lidraughts.app.templating.Environment._
import lidraughts.app.ui.ScalatagsTemplate._
import lidraughts.common.String.html.safeJsonValue

import controllers.routes

object embed {

  import views.html.base.layout.bits._

  def apply(pov: lidraughts.game.Pov, data: play.api.libs.json.JsObject)(implicit ctx: Context) = frag(
    doctype,
    htmlTag(ctx)(
      topComment,
      head(
        charset,
        viewport,
        metaCsp(none),
        st.headTitle(replay titleOf pov),
        pieceSprite(lidraughts.pref.PieceSet.default),
        responsiveCssTag("analyse.round.embed")
      ),
      body(cls := List(
        s"highlight ${ctx.currentBg} ${ctx.currentTheme.cssClass}" -> true
      ))(
        div(cls := "is2d")(
          main(cls := "analyse")
        ),
        footer {
          val url = routes.Round.watcher(pov.gameId, pov.color.name)
          frag(
            div(cls := "left")(
              a(target := "_blank", href := url)(h1(titleGame(pov.game))),
              " ",
              em("brought to you by ", a(target := "_blank", href := netBaseUrl)(netDomain))
            ),
            a(target := "_blank", cls := "open", href := url)("Open")
          )
        },
        jQueryTag,
        jsTag("vendor/mousetrap.js"),
        jsAt("compiled/util.js"),
        jsAt("compiled/trans.js"),
        analyseTag,
        jsTag("embed-analyse.js"),
        embedJs(s"""lidraughts.startEmbeddedAnalyse({
data: ${safeJsonValue(data)},
embed: true,
i18n: ${views.html.board.userAnalysisI18n(withCeval = false, withExplorer = false)}
});""")
      )
    )
  )

  def notFound()(implicit ctx: Context) = frag(
    doctype,
    htmlTag(ctx)(
      topComment,
      head(
        charset,
        viewport,
        metaCsp(none),
        st.headTitle("404 - Game not found"),
        responsiveCssTag("analyse.round.embed")
      ),
      body(cls := ctx.currentBg)(
        div(cls := "not-found")(
          h1("Game not found")
        )
      )
    )
  )
}
