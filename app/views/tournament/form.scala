package views.html
package tournament

import play.api.data.{ Field, Form }

import draughts.variant.{ Variant, Standard, Russian, Brazilian }
import lidraughts.api.Context
import lidraughts.app.templating.Environment._
import lidraughts.app.ui.ScalatagsTemplate._
import lidraughts.tournament.{ Condition, DataForm, Tournament }
import lidraughts.user.User

import controllers.routes

object form {

  def create(form: Form[_], config: DataForm, me: User, teams: List[lidraughts.hub.lightTeam.LightTeam])(implicit ctx: Context) = views.html.base.layout(
    title = trans.newTournament.txt(),
    moreCss = cssTag("tournament.form"),
    moreJs = frag(
      flatpickrTag,
      jsTag("tournamentForm.js")
    )
  ) {
      val isTeamBattle = form("teamBattleByTeam").value.nonEmpty
      val fields = new TourFields(me, form)
      main(cls := "page-small")(
        div(cls := "tour__form box box-pad")(
          h1(
            if (isTeamBattle) "New Team Battle"
            else trans.createANewTournament()
          ),
          postForm(cls := "form3", action := routes.Tournament.create)(
            fields.name(isTeamBattle),
            form3.split(fields.rated, fields.variant),
            fields.startPosition(Standard),
            fields.startPosition(Russian),
            fields.startPosition(Brazilian),
            fields.clock,
            form3.split(
              form3.group(form("minutes"), trans.duration(), half = true)(form3.select(_, DataForm.minuteChoices)),
              form3.group(form("waitMinutes"), trans.timeBeforeTournamentStarts(), half = true)(form3.select(_, DataForm.waitMinuteChoices))
            ),
            fields.description,
            form3.globalError(form),
            fieldset(cls := "conditions")(
              fields.advancedSettings,
              div(cls := "form")(
                !isTeamBattle option fields.password,
                condition(form, auto = true, teams = if (isTeamBattle) Nil else teams),
                fields.berserkableHack,
                fields.streakableHack,
                fields.startDate()
              )
            ),
            isTeamBattle option form3.hidden(form("teamBattleByTeam")),
            form3.actions(
              a(href := routes.Tournament.home())(trans.cancel()),
              form3.submit(trans.createANewTournament(), icon = "g".some)
            )
          )
        ),
        div(cls := "box box-pad tour__faq")(tournament.faq())
      )
    }

  def edit(tour: Tournament, form: Form[_], config: DataForm, me: User, teams: List[lidraughts.hub.lightTeam.LightTeam])(implicit ctx: Context) = views.html.base.layout(
    title = tour.fullName,
    moreCss = cssTag("tournament.form"),
    moreJs = frag(
      flatpickrTag,
      jsTag("tournamentForm.js")
    )
  ) {
      val isTeamBattle = tour.isTeamBattle || form("teamBattleByTeam").value.nonEmpty
      val fields = new TourFields(me, form)
      main(cls := "page-small")(
        div(cls := "tour__form box box-pad")(
          h1("Edit ", tour.fullName),
          postForm(cls := "form3", action := routes.Tournament.update(tour.id))(
            fields.name(isTeamBattle),
            !tour.isStarted option fields.startDate(false),
            form3.split(fields.rated, fields.variant),
            fields.startPosition(Standard),
            fields.startPosition(Russian),
            fields.startPosition(Brazilian),
            fields.clock,
            form3.split(
              if (DataForm.minutes contains tour.minutes) form3.group(form("minutes"), trans.duration(), half = true)(form3.select(_, DataForm.minuteChoices))
              else form3.group(form("minutes"), trans.duration(), half = true)(form3.input(_)(tpe := "number"))
            ),
            fields.description,
            form3.globalError(form),
            fieldset(cls := "conditions")(
              fields.advancedSettings,
              div(cls := "form")(
                !isTeamBattle option fields.password,
                views.html.tournament.form.condition(form, auto = true, teams = if (isTeamBattle) Nil else teams),
                fields.berserkableHack,
                fields.streakableHack
              )
            ),
            isTeamBattle option form3.hidden(form("teamBattleByTeam")),
            form3.actions(
              a(href := routes.Tournament.show(tour.id))(trans.cancel()),
              form3.submit(trans.save(), icon = "g".some)
            )
          ),
          postForm(cls := "terminate", action := routes.Tournament.terminate(tour.id))(
            submitButton(dataIcon := "j".some, cls := "text button button-red confirm")(
              trans.cancelTheTournament()
            )
          )
        )
      )
    }

  private def autoField(auto: Boolean, field: Field)(visible: Field => Frag) = frag(
    if (auto) form3.hidden(field) else visible(field)
  )

  def condition(form: Form[_], auto: Boolean, teams: List[lidraughts.hub.lightTeam.LightTeam])(implicit ctx: Context) = frag(
    form3.split(
      form3.group(form("conditions.nbRatedGame.nb"), raw("Minimum rated games"), half = true)(form3.select(_, Condition.DataForm.nbRatedGameChoices)),
      autoField(auto, form("conditions.nbRatedGame.perf")) { field =>
        form3.group(field, raw("In variant"), half = true)(form3.select(_, ("", "Any") :: Condition.DataForm.perfChoices))
      }
    ),
    form3.split(
      form3.group(form("conditions.minRating.rating"), raw("Minimum rating"), half = true)(form3.select(_, Condition.DataForm.minRatingChoices)),
      autoField(auto, form("conditions.minRating.perf")) { field =>
        form3.group(field, raw("In variant"), half = true)(form3.select(_, Condition.DataForm.perfChoices))
      }
    ),
    form3.split(
      form3.group(form("conditions.maxRating.rating"), raw("Maximum weekly rating"), half = true)(form3.select(_, Condition.DataForm.maxRatingChoices)),
      autoField(auto, form("conditions.maxRating.perf")) { field =>
        form3.group(field, raw("In variant"), half = true)(form3.select(_, Condition.DataForm.perfChoices))
      }
    ),
    form3.split(
      form3.checkbox(form("berserkable"), raw("Allow berserk"), help = raw("Let players halve their clock time to gain an extra point").some, half = true),
      form3.checkbox(form("streakable"), raw("Arena streaks"), help = raw("After 2 wins, consecutive wins grant 4 points instead of 2").some, half = true)
    ),
    form3.split(
      (ctx.me.exists(_.hasTitle) || isGranted(_.ManageTournament)) ?? {
        form3.checkbox(form("conditions.titled"), raw("Only titled players"), help = raw("Require an official title to join the tournament").some, half = true)
      }
    ),
    (auto && teams.nonEmpty) option {
      val baseField = form("conditions.teamMember.teamId")
      val field = ctx.req.queryString get "team" flatMap (_.headOption) match {
        case None => baseField
        case Some(team) => baseField.copy(value = team.some)
      }
      form3.group(field, trans.onlyMembersOfTeam())(
        form3.select(_, List(("", trans.noRestriction.txt())) ::: teams.map(_.pair))
      )
    }
  )

  def startingPosition(field: Field, variant: Variant)(implicit ctx: Context) = st.select(
    id := form3.id(field),
    name := field.name,
    cls := "form-control"
  )(
      option(
        value := variant.initialFen,
        field.value.has(variant.initialFen) option selected
      )(trans.startPosition()),
      variant.openingTables.map { table =>
        val key = table.withRandomFen
        option(
          value := key,
          field.value.has(key) option selected
        )(trans.randomOpeningFromX(table.name))
      },
      variant.openingTables.flatMap { table =>
        table.categories.map { categ =>
          optgroup(attr("label") := s"${categ.name} - ${table.name}")(
            categ.positions.map { v =>
              val key = table.withFen(v)
              option(value := key, field.value.has(key) option selected)(v.fullName)
            }
          )
        }
      }
    )
}

final private class TourFields(me: User, form: Form[_])(implicit ctx: Context) {

  def name(isTeamBattle: Boolean) = DataForm.canPickName(me) ?? {
    form3.group(form("name"), trans.name()) { f =>
      div(
        form3.input(f), " ", if (isTeamBattle) "Team Battle" else "Arena", br,
        small(cls := "form-help")(
          trans.safeTournamentName(), br,
          trans.inappropriateNameWarning(), br,
          trans.emptyTournamentName()
        )
      )
    }
  }

  def rated = frag(
    form3.checkbox(
      form("rated"),
      trans.rated(),
      help = trans.gamesWillImpactThePlayersRating().some
    ),
    st.input(tpe := "hidden", st.name := form("rated").name, value := "false") // hack allow disabling rated
  )
  def variant =
    form3.group(form("variant"), trans.variant(), half = true)(
      form3.select(_, translatedVariantChoicesWithVariants.map(x => x._1 -> x._2))
    )
  def startPosition(v: Variant) =
    form3.group(form("position_" + v.key), trans.startPosition(), klass = "position position-" + v.key)(
      views.html.tournament.form.startingPosition(_, v)
    )
  def clock =
    form3.split(
      form3.group(form("clockTime"), trans.clockInitialTime(), half = true)(
        form3.select(_, DataForm.clockTimeChoices)
      ),
      form3.group(form("clockIncrement"), trans.clockIncrement(), half = true)(
        form3.select(_, DataForm.clockIncrementChoices)
      )
    )
  def description =
    form3.group(form("description"), trans.tournamentDescription(), help = trans.tournamentDescriptionHelp().some)(
      form3.textarea(_)(rows := 4)
    )
  def password =
    form3.group(form("password"), trans.password(), help = trans.makePrivateTournament().some)(
      form3.input(_)(autocomplete := "off")
    )
  def berserkableHack =
    input(tpe := "hidden", st.name := form("berserkable").name, value := "false") // hack allow disabling berserk
  def streakableHack =
    input(tpe := "hidden", st.name := form("streakable").name, value := "false") // hack allow disabling streaks
  def startDate(withHelp: Boolean = true) =
    form3.group(
      form("startDate"),
      raw("Custom start date"),
      help = withHelp option raw("""This overrides the "Time before tournament starts" setting""")
    )(form3.flatpickr(_))
  def advancedSettings = frag(
    legend(trans.advancedSettings()),
    errMsg(form("conditions")),
    p(
      strong(dataIcon := "!", cls := "text")(trans.recommendNotTouching()),
      " ",
      trans.fewerPlayers(),
      " ",
      a(cls := "show")(trans.showAdvancedSettings())
    )
  )
}