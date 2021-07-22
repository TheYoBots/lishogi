package lishogi.db

import reactivemongo.api._

import scala.concurrent.duration._
import scala.concurrent.Future

import dsl.Coll
import lishogi.common.Chronometer
import lishogi.common.config.CollName

final class AsyncDb(
    name: String,
    uri: String,
    driver: AsyncDriver
)(implicit ec: scala.concurrent.ExecutionContext) {

  private lazy val connection =
    MongoConnection.fromString(uri) flatMap { parsedUri =>
      driver.connect(parsedUri, name.some).dmap(_ -> parsedUri.db)
    }

  private def db: Future[DB] =
    connection flatMap { case (conn, dbName) =>
      conn database dbName.getOrElse("lishogi")
    }

  def apply(name: CollName) = new AsyncColl(name, () => db.dmap(_(name.value)))
}

final class Db(
    name: String,
    uri: String,
    driver: AsyncDriver
)(implicit ec: scala.concurrent.ExecutionContext) {

  private val logger = lishogi.db.logger branch name

  private lazy val db: DB = Chronometer.syncEffect(
    MongoConnection
      .fromString(uri)
      .flatMap { parsedUri =>
        driver
          .connect(parsedUri, name.some)
          .flatMap(_ database parsedUri.db.getOrElse("lishogi"))
      }
      .await(5.seconds, s"db:$name")
  ) { lap =>
    logger.info(s"MongoDB connected to $uri in ${lap.showDuration}")
  }

  def apply(name: CollName): Coll = db(name.value)

  val runCommand = new RunCommand({ (command, readPreference) =>
    db.runCommand(command, FailoverStrategy.strict)
      .one[dsl.Bdoc](readPreference)
  })
}
