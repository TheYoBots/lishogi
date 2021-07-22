package lishogi.app
package templating

import lishogi.common.paginator.Paginator

trait PaginatorHelper {

  implicit def toRichPager[A](pager: Paginator[A]): RichPager = new RichPager(pager)
}

final class RichPager(pager: Paginator[_]) {

  def sliding(length: Int, showPost: Boolean = true): List[Option[Int]] = {
    val fromPage = 1 max (pager.currentPage - length)
    val toPage   = pager.nbPages min (pager.currentPage + length)
    val pre = fromPage match {
      case 1 => Nil
      case 2 => List(1.some)
      case _ => List(1.some, none)
    }
    val post = toPage match {
      case x if x == pager.nbPages     => Nil
      case x if x == pager.nbPages - 1 => List(pager.nbPages.some)
      case _ if showPost               => List(none, pager.nbPages.some)
      case _                           => List(none)
    }
    pre ::: (fromPage to toPage).view.map(some).toList ::: post
  }
}
