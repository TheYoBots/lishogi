package views.html.mod

import play.api.i18n.Lang

import lishogi.api.Context
import lishogi.app.templating.Environment._
import lishogi.app.ui.ScalatagsTemplate._
import lishogi.mod.Gamify.Period

import controllers.routes

object gamify {

  def index(leaderboards: lishogi.mod.Gamify.Leaderboards, history: List[lishogi.mod.Gamify.HistoryMonth])(implicit
      ctx: Context
  ) = {
    val title = "Moderator hall of fame"
    def yearHeader(year: Int) =
      tr(cls := "year")(
        th(year),
        th("Champions of the past"),
        th("Score"),
        th("Actions taken"),
        th("Report points")
      )

    views.html.base.layout(
      title = title,
      moreCss = cssTag("mod.gamify")
    ) {
      main(cls := "page-menu")(
        views.html.mod.menu("gamify"),
        div(id := "mod-gamify", cls := "page-menu__content index box")(
          h1(title),
          div(cls := "champs")(
            champion(leaderboards.daily.headOption, "reward1", Period.Day),
            champion(leaderboards.weekly.headOption, "reward2", Period.Week),
            champion(leaderboards.monthly.headOption, "reward3", Period.Month)
          ),
          table(cls := "slist slist-pad history")(
            tbody(
              history.headOption.filterNot(_.date.getMonthOfYear == 12).map { h =>
                yearHeader(h.date.getYear)
              },
              history.map { h =>
                frag(
                  h.date.getMonthOfYear == 12 option yearHeader(h.date.getYear),
                  tr(
                    th(h.date.monthOfYear.getAsText),
                    th(userIdLink(h.champion.modId.some, withOnline = false)),
                    td(cls := "score")(h.champion.score.localize),
                    td(h.champion.action.localize),
                    td(h.champion.report.localize)
                  )
                )
              }
            )
          )
        )
      )
    }
  }

  def period(leaderboards: lishogi.mod.Gamify.Leaderboards, period: lishogi.mod.Gamify.Period)(implicit
      ctx: Context
  ) = {
    val title = s"Moderators of the ${period.name}"
    views.html.base.layout(
      title = title,
      moreCss = cssTag("mod.gamify")
    ) {
      main(cls := "page-menu")(
        views.html.mod.menu("gamify"),
        div(id := "mod-gamify", cls := "page-menu__content box")(
          h1(
            a(href := routes.Mod.gamify(), dataIcon := "I"),
            title
          ),
          div(cls := "period")(
            table(cls := "slist")(
              thead(
                tr(
                  th(colspan := "2"),
                  th("Actions"),
                  th("Reports"),
                  th("Score")
                )
              ),
              tbody(
                leaderboards(period).zipWithIndex.map { case (m, i) =>
                  tr(
                    th(i + 1),
                    th(userIdLink(m.modId.some, withOnline = false)),
                    td(m.action.localize),
                    td(m.report.localize),
                    td(cls := "score")(m.score.localize)
                  )
                }
              )
            )
          )
        )
      )
    }
  }

  def champion(champ: Option[lishogi.mod.Gamify.ModMixed], img: String, period: lishogi.mod.Gamify.Period)(implicit
      lang: Lang
  ) =
    div(cls := "champ")(
      st.img(src := staticUrl(s"images/mod/$img.png")),
      h2("Mod of the ", period.name),
      champ.map { m =>
        frag(
          userIdLink(m.modId.some, withOnline = false),
          table(
            tbody(
              tr(
                th("Total score"),
                td(m.score)
              ),
              tr(
                th("Actions taken"),
                td(m.action)
              ),
              tr(
                th("Report points"),
                td(m.report)
              )
            )
          )
        )
      } getOrElse "Nobody!",
      a(cls := "button button-empty", href := routes.Mod.gamifyPeriod(period.name))(
        "View ",
        period.name,
        " leaderboard"
      )
    )
}
