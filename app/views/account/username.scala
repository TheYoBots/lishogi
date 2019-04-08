package views.html
package account

import lidraughts.api.Context
import lidraughts.app.templating.Environment._
import lidraughts.app.ui.ScalatagsTemplate._

import controllers.routes

object username {

  def apply(u: lidraughts.user.User, form: play.api.data.Form[_])(implicit ctx: Context) = account.layout(
    title = s"${u.username} - ${trans.changeUsername.txt()}",
    active = "username"
  ) {
    div(cls := "account box box-pad")(
      h1(cls := "text")(trans.changeUsername.frag()),
      st.form(cls := "form3", action := routes.Account.usernameApply, method := "POST")(
        form3.globalError(form),
        form3.group(form("username"), trans.username.frag(), help = trans.changeUsernameDescription.frag().some)(form3.input(_)),
        form3.action(form3.submit(trans.apply.frag()))
      )
    )
  }
}
