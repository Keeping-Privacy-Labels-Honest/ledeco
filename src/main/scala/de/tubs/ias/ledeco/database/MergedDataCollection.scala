package de.tubs.ias.ledeco.database
import de.tubs.ias.ledeco.database.MergedDataCollection.BadCollection
import de.tubs.ias.ledeco.database.entities.{
  CellphoneApplication,
  Collection,
  Cookie,
  FastRequestsExtraction,
  Header,
  Request
}
import scalikejdbc.{DBSession, scalikejdbcSQLInterpolationImplicitDef}
import wvlet.log.LogSupport

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}
import scala.util.Random

case class NumericCollectionIdentifyer(id: Int) extends Identifyer

object MergedDataCollection {

  def getRequestSetA: Seq[Request] = {
    val int = Random.nextInt()
    List(
      Request(null,
              "POST",
              "facebook.com",
              "/some/path/",
              s"id:$int",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List()),
      Request(null,
              "POST",
              "facebook.com",
              s"/some/endpoint?id=$int&other=12345",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List()),
      Request(null,
              "POST",
              "facebook.com",
              s"/some/endpoint?id=$int&other=4248",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List()),
      Request(null,
              "GET",
              "chartboost.com",
              "/endpoint",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List(Header("tracking", s"$int"))),
      Request(null,
              "GET",
              "chartboost.com",
              "/endpoint",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List(Header("tracking", s"$int"))),
    )
  }

  def getRequestSetB: Seq[Request] = {
    val int = Random.nextInt()
    List(
      Request(null,
              "POST",
              "facebook.com",
              "/some/path/",
              s"id:$int",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List()),
      Request(null,
              "POST",
              "flurry.com",
              "/tracking/endpoint.php",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(Cookie("id", s"$int")),
              List()),
      Request(null,
              "GET",
              "chartboost.com",
              "/endpoint",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List(Header("tracking", s"$int"))),
      Request(null,
              "GET",
              "chartboost.com",
              "/endpoint",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List(Header("tracking", s"$int"))),
      Request(null,
              "POST",
              "facebook.com",
              "/some/path/",
              s"id:$int",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(),
              List()),
      Request(null,
              "POST",
              "flurry.com",
              "/tracking/endpoint.php",
              "",
              "443",
              "HTTP",
              "some",
              "1.1",
              List(Cookie("id", s"$int")),
              List()),
      Request(null,
              "POST",
              "ati-host.net",
              s"/endpoint?id=$int",
              "",
              "",
              "",
              "",
              "",
              List(),
              List())
    )
  }

  def getDummyCollection: Collection = {
    Collection(
      42,
      "DUMMY",
      null,
      null,
      Seq(
        CellphoneApplication("Some", "App") -> getRequestSetA,
        CellphoneApplication("Other", "App") -> getRequestSetB,
        CellphoneApplication("Really", "App") -> getRequestSetA,
        CellphoneApplication("Dummy", "App") -> getRequestSetA,
        CellphoneApplication("Debug", "App") -> getRequestSetB,
      )
    )
  }

  type BadCollection = Seq[(CellphoneApplication, String)]

}

class MergedDataCollection(connection: PostgresConnection)
    extends DataCollection
    with LogSupport {

  override def getCollection(id: Identifyer,
                             restrictions: Option[Seq[CellphoneApplication]])
    : (Collection, BadCollection) = {
    assert(id.isInstanceOf[NumericCollectionIdentifyer])
    val collectionId = id.asInstanceOf[NumericCollectionIdentifyer].id
    logger.info(s"getting collection with id #$collectionId")
    val (phone, startTime, endTime) = connection.withSession {
      implicit session: DBSession =>
        val (phone, start, end) =
          sql"""SELECT phone, start_time, end_time FROM collections WHERE id = $collectionId"""
            .map { entity =>
              (entity.string("phone"),
               entity.dateTime("start_time"),
               entity.dateTime("end_time"))
            }
            .first()
            .apply()
            .get
        (phone, start, end)
    }
    val (validMonitorings, errorMonitorings) =
      getMonitorings(id.asInstanceOf[NumericCollectionIdentifyer].id,
                     restrictions)
    val collection =
      Collection(collectionId, phone, startTime, endTime, validMonitorings)
    (collection, errorMonitorings)
  }

  def getMonitorings(collectionId: Int,
                     restrictions: Option[Seq[CellphoneApplication]])
    : (Seq[(CellphoneApplication, Seq[Request])],
       Seq[(CellphoneApplication, String)]) = {
    val premonitorings: Seq[(Int, String, String, Option[String])] =
      connection.withSession { implicit session: DBSession =>
        sql"""SELECT id, app_name, app_version, error FROM AppMonitorings WHERE collection = $collectionId"""
          .map { entity =>
            (entity.int("id"),
             entity.string("app_name"),
             entity.string("app_version"),
             entity.stringOpt("error"))
          }
          .list()
          .apply()
      }
    val monitorings = restrictions match {
      case None => premonitorings
      case Some(apps) =>
        info(s"applying filtering")
        val appSet = apps.toSet
        premonitorings.filter {
          case (_, name, version, _) =>
            appSet.contains(CellphoneApplication(name, version))
        }
    }
    val good = monitorings.filter(_._4.isEmpty)
    val bad = monitorings
      .filter(_._4.nonEmpty)
      .map(elem => (CellphoneApplication(elem._2, elem._3), elem._4.get))
    info(
      s"we have ${bad.length} monitorings that encountered an error out of ${monitorings.length}")
    info(s"retrieving ${monitorings.size} monitorings from the database")
    val requestMap = connection.withSession { implicit session: DBSession =>
      FastRequestsExtraction.getRequests(good.map(_._1))
    }
    val requestFuture = Future.sequence {
      good.map { monitoring =>
        Future {
          CellphoneApplication(monitoring._2, monitoring._3) -> requestMap
            .getOrElse(monitoring._1, List())
        }
      }
    }
    (Await.result(requestFuture, Inf), bad)
  }

  private def getRequests(monitoringId: Int)(
      implicit session: DBSession): Seq[Request] = {
    sql"""SELECT id,
                   start_time,
                   scheme,
                   method,
                   host,
                   port,
                   path,
                   content,
                   authority,
                   http_version
            FROM Requests WHERE monitoring_id = $monitoringId
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

  def importCollection(
      collection: Collection,
      bad: Option[Seq[(CellphoneApplication, String)]] = None): Int = {
    connection.withSession { implicit session: DBSession =>
      val id = {
        collection.phone match {
          case "iphone" =>
            sql"""INSERT INTO COLLECTIONS (phone,start_time,end_time)
                VALUES ('iphone',${collection.start},${collection.end})
                RETURNING id
                """.map(_.int("id")).first().apply().get
          case "android" =>
            sql"""INSERT INTO COLLECTIONS (phone,start_time,end_time)
                VALUES ('android',${collection.start},${collection.end})
                RETURNING id
                """.map(_.int("id")).first().apply().get
        }
      }
      importMonitoring(id, collection.monitoring)
      bad match {
        case Some(badMonitorings) => importBadMonitorings(id, badMonitorings)
        case None                 =>
      }
      id
    }
  }

  private def importMonitoring(
      collection: Int,
      monitoring: Seq[(CellphoneApplication, Seq[Request])])(
      implicit session: DBSession): Unit = {
    monitoring.foreach {
      case (app, requests) =>
        importApp(app)
        val id =
          sql"""INSERT INTO AppMonitorings (collection, app_name, app_version) VALUES ($collection, ${app.name}, ${app.version}) RETURNING id"""
            .map(_.int("id"))
            .first()
            .apply()
            .get
        importRequests(id, requests)
    }
  }

  private def importBadMonitorings(
      collection: Int,
      monitorings: Seq[(CellphoneApplication, String)])(
      implicit session: DBSession): Unit = {
    monitorings.foreach {
      case (app, error) =>
        importApp(app)
        sql"""INSERT INTO AppMonitorings (collection, app_name, app_version, error) VALUES ($collection, ${app.name}, ${app.version}, $error)"""
          .execute()
          .apply()
    }
  }

  private def importApp(app: entities.CellphoneApplication)(
      implicit session: DBSession): Unit = {
    sql"""INSERT INTO APPS(name, version) VALUES (${app.name},${app.version}) ON CONFLICT DO NOTHING"""
      .execute()
      .apply()
  }

  private def importRequests(monitoring: Int, request: Seq[Request])(
      implicit session: DBSession): Unit = {
    request.foreach { request =>
      val id =
        sql"""INSERT INTO REQUESTS (monitoring_id, start_time, scheme, method, host, port, path, content, authority, http_version)
                         VALUES ($monitoring, ${request.start}, ${request.scheme},
                                 ${request.method}, ${request.host}, ${request.port}, ${request.getPath}, ${request.content},
                                 ${request.authority},${request.http_version})
                         RETURNING id""".map(_.int("id")).first().apply().get
      importCookies(id, request.cookies)
      importHeaders(id, request.headers)
    }
  }

  private def importCookies(request: Int, cookies: Seq[Cookie])(
      implicit session: DBSession): Unit = {
    cookies.foreach { cookie =>
      sql"""INSERT INTO COOKIES (request, name, values) VALUES ($request, ${cookie.name}, ${cookie.values})"""
        .execute()
        .apply()
    }
  }

  private def importHeaders(request: Int, headers: Seq[Header])(
      implicit session: DBSession): Unit = {
    headers.foreach { header =>
      sql"""INSERT INTO HEADERS (request, name, values) VALUES ($request, ${header.name}, ${header.values})"""
        .execute()
        .apply()
    }
  }

}
