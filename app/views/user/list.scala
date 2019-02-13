package views.html
package user

import lidraughts.api.Context
import lidraughts.app.templating.Environment._
import lidraughts.app.ui.ScalatagsTemplate._
import lidraughts.rating.PerfType
import lidraughts.user.User

import controllers.routes

object list {

  def apply(
    tourneyWinners: List[lidraughts.tournament.Winner],
    online: List[User],
    leaderboards: lidraughts.user.Perfs.Leaderboards,
    nbDay: List[User.LightCount],
    nbAllTime: List[User.LightCount]
  )(implicit ctx: Context) = views.html.base.layout(
    title = trans.players.txt(),
    moreCss = responsiveCssTag("user.list"),
    responsive = true,
    fullScreen = true,
    openGraph = lidraughts.app.ui.OpenGraph(
      title = "Draughts players and leaderboards",
      url = s"$netBaseUrl${routes.User.list.url}",
      description = "Best draughts players in bullet, blitz, rapid, classical, Frisian, Antidraughts and more draughts variants"
    ).some
  ) {
      main(cls := "page-menu page-large")(
        bits.communityMenu("leaderboard"),
        div(cls := "community page-menu__content box box-pad")(
          st.section(cls := "community__online")(
            h2(trans.onlinePlayers.frag()),
            ol(cls := "user_top")(online map { u =>
              li(
                userLink(u),
                showBestPerf(u)
              )
            })
          ),
          div(cls := "community__leaders")(
            h2(trans.leaderboard.frag()),
            div(cls := "leaderboards")(
              userTopPerf(leaderboards.bullet, PerfType.Bullet),
              userTopPerf(leaderboards.blitz, PerfType.Blitz),
              userTopPerf(leaderboards.rapid, PerfType.Rapid),
              userTopPerf(leaderboards.classical, PerfType.Classical),
              userTopPerf(leaderboards.ultraBullet, PerfType.UltraBullet),
              
              userTopActive(nbAllTime, trans.activePlayers(), icon = 'U'.some),
              tournamentWinners(tourneyWinners),

              userTopPerf(leaderboards.frisian, PerfType.Frisian),
              userTopPerf(leaderboards.antidraughts, PerfType.Antidraughts),
              userTopPerf(leaderboards.breakthrough, PerfType.Breakthrough),
              userTopPerf(leaderboards.frysk, PerfType.Frysk)
            )
          )
        )
      )
    }

  private def tournamentWinners(winners: List[lidraughts.tournament.Winner])(implicit ctx: Context) =
    st.section(cls := "user_top")(
      h2(cls := "text", dataIcon := "g")(
        a(href := routes.Tournament.leaderboard)(trans.tournament.frag())
      ),
      ol(winners take 10 map { w =>
        li(
          userIdLink(w.userId.some),
          a(title := w.tourName, href := routes.Tournament.show(w.tourId))(
            scheduledTournamentNameShortHtml(w.tourName)
          )
        )
      })
    )

  private def userTopPerf(users: List[User.LightPerf], perfType: PerfType) =
    st.section(cls := "user_top")(
      h2(cls := "text", dataIcon := perfType.iconChar)(
        a(href := routes.User.topNb(200, perfType.key))(perfType.name)
      ),
      ol(users map { l =>
        li(
          lightUserLink(l.user),
          l.rating
        )
      })
    )

  private def userTopActive(users: List[User.LightCount], hTitle: Any, icon: Option[Char] = None)(implicit ctx: Context) =
    st.section(cls := "user_top")(
      h2(cls := "text", dataIcon := icon.map(_.toString))(hTitle.toString),
      ol(users map { u =>
        li(
          lightUserLink(u.user),
          span(title := trans.gamesPlayed.txt())(s"#${u.count.localize}")
        )
      })
    )
}
