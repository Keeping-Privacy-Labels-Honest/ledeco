package de.tubs.ias.ledeco.database
import de.tubs.ias.ledeco.database.MergedDataCollection.BadCollection
import de.tubs.ias.ledeco.database.entities.{
  CellphoneApplication,
  Collection,
  Cookie,
  Header,
  Request
}
import scalikejdbc.{DBSession, scalikejdbcSQLInterpolationImplicitDef}
import wvlet.log.LogSupport

import java.time.ZonedDateTime
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf

object NoIdentifyer extends Identifyer

class AndroidDatabase(connection: PostgresConnection)
    extends DataCollection
    with LogSupport {

  override def getCollection(id: Identifyer,
                             appRestrictions: Option[Seq[CellphoneApplication]])
    : (Collection, BadCollection) = {
    assert(id == NoIdentifyer,
           "the underlying schema does not require a run id")
    val (runIds, startTime, endTime, runAppMap) = connection.withSession {
      implicit session =>
        val runAppMap = getRunAppMap
        val runIds =
          sql"""SELECT id FROM runs""".map(_.int("id")).list().apply()
        val (start, end) = getStartEndTimes
        (runIds, start, end, runAppMap)
    }
    val future = Future.sequence {
      runIds.map { runId =>
        Future {
          connection.withSession { implicit session: DBSession =>
            runAppMap(runId) -> getRequests(runId)
          }
        }
      }
    }
    (Collection(-1, "android", startTime, endTime, Await.result(future, Inf)),
     Nil)
  }

  private def getStartEndTimes(
      implicit session: DBSession): (ZonedDateTime, ZonedDateTime) = {
    sql"""SELECT MIN(start_time) as startTime, MAX(end_time) AS endTime FROM runs"""
      .map { entity =>
        (entity.dateTime("startTime"), entity.dateTime("endTime"))
      }
      .list()
      .first()
      .apply()
      .get
  }

  private def getRequests(runId: Int)(
      implicit session: DBSession): Seq[Request] = {
    sql"""SELECT id,
                     run,
                     start_time,
                     method,
                     host,
                     path,
                     content,
                     port,
                     scheme,
                     authority,
                     http_version
              FROM requests
              WHERE run = $runId
           """
      .map { entity =>
        Request(
          entity.dateTime("start_time"),
          entity.string("method"),
          entity.string("host"),
          entity.string("path"),
          entity.string("content"),
          entity.string("port"),
          entity.string("scheme"),
          entity.string("authority"),
          entity.string("http_version"),
          getCookies(entity.int("id")).toList,
          getHeaders(entity.int("id")).toList,
        )
      }
      .list()
      .apply()
  }

  private def getRunAppMap(
      implicit session: DBSession): Map[Int, entities.CellphoneApplication] = {
    sql"""SELECT runs.id AS run,
                 apps.name AS name,
                 apps.version AS version
          FROM runs JOIN apps ON apps.name = runs.app
       """
      .map { entity =>
        entity.int("run") ->
          CellphoneApplication(
            entity.string("name"),
            entity.string("version")
          )
      }
      .list()
      .apply()
      .toMap
  }

  private def getCookies(request: Int)(
      implicit session: DBSession): Seq[Cookie] = {
    sql"""SELECT name, values
          FROM cookies
          WHERE request = $request
       """
      .map { entity =>
        Cookie(
          entity.string("name"),
          entity.string("values")
        )
      }
      .list()
      .apply()
  }

  private def getHeaders(request: Int)(
      implicit session: DBSession): Seq[Header] = {
    sql"""SELECT name, values
          FROM headers
          WHERE request = $request
       """
      .map { entity =>
        Header(
          entity.string("name"),
          entity.string("values")
        )
      }
      .list()
      .apply()
  }
}
