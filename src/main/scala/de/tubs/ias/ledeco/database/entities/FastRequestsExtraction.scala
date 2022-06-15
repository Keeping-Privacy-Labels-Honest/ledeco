package de.tubs.ias.ledeco.database.entities

import scalikejdbc.{DBSession, scalikejdbcSQLInterpolationImplicitDef}
import wvlet.log.LogSupport

import java.time.ZonedDateTime
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

object FastRequestsExtraction extends LogSupport {

  def getRequests(monitoringIds: Seq[Int])(
      implicit session: DBSession): Map[Int, Seq[Request]] = {
    info(
      s"creating internal request map for ${monitoringIds.length} monitorings")
    val requestMap: Map[Int,
                        List[
                          (Int,
                           Int,
                           ZonedDateTime,
                           String,
                           String,
                           String,
                           String,
                           String,
                           String,
                           String,
                           String)]] = {
      sql"""SELECT monitoring_id,
            id,
            start_time,
            scheme,
            method,
            host,
            port,
            path,
            content,
            authority,
            http_version
            FROM Requests WHERE monitoring_id IN ($monitoringIds)"""
        .map { entity =>
          (entity.int("monitoring_id"),
           entity.int("id"),
           entity.dateTime("start_time"),
           entity.string("method"),
           entity.string("host"),
           entity.string("path"),
           entity.string("content"),
           entity.string("port"),
           entity.string("scheme"),
           entity.string("authority"),
           entity.string("http_version"))
        }
        .list()
        .apply()
        .groupBy(_._1)
        .toMap
    }
    val requestIds = requestMap.flatMap(_._2.map(_._2)).toSet
    info(s"creating cookie map with ${requestIds.size} requests")
    val cookieMap: Map[Int, List[(Int, String, String)]] =
      sql"""SELECT request,name,values FROM cookies WHERE request IN ($requestIds)"""
        .map { entity =>
          (entity.int("request"),
           entity.string("name"),
           entity.string("values"))
        }
        .list()
        .apply()
        .groupBy(_._1)
    info(s"creating header map with ${requestIds.size} requests")
    val headerMap: Map[Int, List[(Int, String, String)]] = {
      sql"""SELECT request, name, values FROM headers WHERE request IN ($requestIds)"""
        .map { entity =>
          (entity.int("request"),
           entity.string("name"),
           entity.string("values"))
        }
        .list()
        .apply()
        .groupBy(_._1)
    }
    val future = Future.sequence {
      requestMap.map {
        case (monitoring_id, attributeLists) =>
          Future {
            attributeLists.map {
              case (_,
                    id,
                    start_time,
                    method,
                    host,
                    path,
                    content,
                    port,
                    scheme,
                    authority,
                    http_version) =>
                monitoring_id ->
                  Request(
                    start_time,
                    method,
                    host,
                    path,
                    content,
                    port,
                    scheme,
                    authority,
                    http_version,
                    cookieMap.getOrElse(id, List()).map {
                      case (_, name, value) => Cookie(name, value)
                    },
                    headerMap.getOrElse(id, List()).map {
                      case (_, name, value) => Header(name, value)
                    }
                  )
            }
          }
      }
    }
    Await
      .result(future, Inf)
      .flatten
      .groupBy(_._1)
      .map(elem => elem._1 -> elem._2.map(_._2).toList)
  }

}
