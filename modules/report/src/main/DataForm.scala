package lishogi.report

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._
import scala.concurrent.duration._

import lishogi.common.LightUser
import lishogi.user.User

final private[report] class DataForm(
    lightUserAsync: LightUser.Getter,
    val captcher: lishogi.hub.actors.Captcher,
    domain: lishogi.common.config.NetDomain
)(implicit ec: scala.concurrent.ExecutionContext)
    extends lishogi.hub.CaptchedForm {
  val cheatLinkConstraint: Constraint[ReportSetup] = Constraint("constraints.cheatgamelink") { setup =>
    if (setup.reason != "cheat" || (domain.value + """/(\w{8}|\w{12})""").r.findFirstIn(setup.text).isDefined)
      Valid
    else
      Invalid(Seq(ValidationError("error.provideOneCheatedGameLink")))
  }

  val create = Form(
    mapping(
      "username" -> lishogi.user.DataForm.historicalUsernameField.verifying(
        "Unknown username", {
          blockingFetchUser(_).isDefined
        }
      ),
      "reason" -> text.verifying("error.required", Reason.keys contains _),
      "text"   -> text(minLength = 5, maxLength = 2000),
      "gameId" -> text,
      "move"   -> text
    )({ case (username, reason, text, gameId, move) =>
      ReportSetup(
        user = blockingFetchUser(username) err "Unknown username " + username,
        reason = reason,
        text = text,
        gameId = gameId,
        move = move
      )
    })(_.export.some).verifying(captchaFailMessage, validateCaptcha _).verifying(cheatLinkConstraint)
  )

  def createWithCaptcha = withCaptcha(create)

  val flag = Form(
    mapping(
      "username" -> lishogi.user.DataForm.historicalUsernameField,
      "resource" -> nonEmptyText,
      "text"     -> text(minLength = 3, maxLength = 140)
    )(ReportFlag.apply)(ReportFlag.unapply)
  )

  private def blockingFetchUser(username: String) =
    lightUserAsync(User normalize username).await(1 second, "reportUser")
}

private[report] case class ReportFlag(
    username: String,
    resource: String,
    text: String
)

private[report] case class ReportSetup(
    user: LightUser,
    reason: String,
    text: String,
    gameId: String,
    move: String
) {

  def suspect = SuspectId(user.id)

  def export = (user.name, reason, text, gameId, move)
}
