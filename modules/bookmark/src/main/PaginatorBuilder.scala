package lishogi.bookmark

import lishogi.common.paginator._
import lishogi.db.dsl._
import lishogi.game.GameRepo
import lishogi.user.User

final class PaginatorBuilder(
    coll: Coll,
    gameRepo: GameRepo,
    maxPerPage: lishogi.common.config.MaxPerPage
)(implicit ec: scala.concurrent.ExecutionContext) {

  def byUser(user: User, page: Int): Fu[Paginator[Bookmark]] =
    paginator(new UserAdapter(user), page)

  private def paginator(adapter: AdapterLike[Bookmark], page: Int): Fu[Paginator[Bookmark]] =
    Paginator(
      adapter,
      currentPage = page,
      maxPerPage = maxPerPage
    )

  final class UserAdapter(user: User) extends AdapterLike[Bookmark] {

    def nbResults: Fu[Int] = coll countSel selector

    def slice(offset: Int, length: Int): Fu[Seq[Bookmark]] =
      for {
        gameIds <-
          coll
            .find(selector, $doc("g" -> true).some)
            .sort(sorting)
            .skip(offset)
            .cursor[Bdoc]()
            .list(length) dmap { _ flatMap { _ string "g" } }
        games <- gameRepo gamesFromSecondary gameIds
      } yield games map { g =>
        Bookmark(g, user)
      }

    private def selector = $doc("u" -> user.id)
    private def sorting  = $sort desc "d"
  }
}
