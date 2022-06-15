package de.tubs.ias.ledeco.database.entities

import de.halcony.StringTable

import java.time.ZonedDateTime

object Request {

  val columnSize: Int = 50

  def visuallyTableCompareSimilarRequestsParameter(
      requests: Seq[Request]): String = {
    val parameter =
      requests.flatMap(_.parameter.keys).toSet.toList.filter(_ != "")
    val table =
      new StringTable(List("path") ++ parameter, maxWidth = columnSize)
    requests.foreach { request =>
      val row = List(request.path) ++ parameter.map { param =>
        request.parameter.getOrElse(param, "N/A")
      }
      table.addRow(row)
    }
    if (parameter.nonEmpty) {
      table.getTable
    } else {
      ""
    }
  }

  def visuallyTableCompareSimilarRequestsHeader(
      requests: Seq[Request]): String = {
    val header = requests.flatMap(_.headerMap.keys).toSet.toList
    val table = new StringTable(header, maxWidth = columnSize)
    requests.foreach { request =>
      val row = header.map { header =>
        request.headerMap.getOrElse(header, "N/A")
      }
      table.addRow(row)
    }
    if (header.nonEmpty) {
      table.getTable
    } else {
      ""
    }
  }

  def visuallyTableCompareSimilarRequestsCookies(
      requests: Seq[Request]): String = {
    val cookies = requests.flatMap(_.cookieMap.keys).toSet.toList
    val table = new StringTable(cookies, maxWidth = columnSize)
    requests.foreach { request =>
      val row = cookies.map { cookie =>
        request.cookieMap.getOrElse(cookie, "N/A")
      }
      table.addRow(row)
    }
    if (cookies.nonEmpty) {
      table.getTable
    } else {
      ""
    }
  }

  def visuallyTableCompareSimilarRequestsContent(
      requests: Seq[Request]): String = {
    if (requests.exists(_.content != "")) {
      requests
        .map(_.content)
        .mkString("", "\n---------------------------\n", "")
    } else {
      ""
    }
  }

}

case class Request(start: ZonedDateTime,
                   method: String,
                   host: String,
                   pathAndParameter: String,
                   content: String,
                   port: String,
                   scheme: String,
                   authority: String,
                   http_version: String,
                   cookies: List[Cookie],
                   headers: List[Header]) {

  private val (path, parameter): (String, Map[String, String]) = {
    val split = pathAndParameter.split("\\?")
    split.toList match {
      case Nil => throw new RuntimeException("This should never happen")
      case path :: Nil =>
        (path, Map[String, String]())
      case path :: rest =>
        (path,
         rest
           .mkString("\\?")
           .split("&")
           .map { elem =>
             elem.split("=").toList match {
               case name :: rest => Some(name -> rest.mkString("="))
               case Nil          => None
               case x =>
                 throw new RuntimeException(
                   s"this should never happen weird url param $x")
             }
           }
           .filter(_.nonEmpty)
           .map(_.get)
           .toMap)
    }
  }

  private val headerMap: Map[String, String] =
    headers.map(elem => elem.name -> elem.values).toMap
  private val cookieMap: Map[String, String] =
    cookies.map(elem => elem.name -> elem.values).toMap

  def getParameters: Parameters =
    Parameters(cookieMap, parameter, headerMap, Map())

  def getPath: String = path

  def getCookieMap: Map[String, String] = cookieMap

  def getHeaderMap: Map[String, String] = headerMap

  def getRequestUrl(pathp: Boolean = false): String =
    if (pathp) {
      s"$scheme://$host$path"
    } else {
      s"$scheme://$host"
    }

}
