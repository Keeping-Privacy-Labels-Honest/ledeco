package de.tubs.ias.ledeco.utility

import com.nimbusds.jose.util.StandardCharset
import de.tubs.ias.ledeco.analysis._
import de.tubs.ias.ledeco.database.entities.Request
import de.tubs.ias.ledeco.tracker.{ExodusTracker, IdentifiedTracker}
import de.tubs.ias.ledeco.utility.RequestLocation.{
  Body,
  Cookie,
  Location,
  Mixed,
  URL
}
import spray.json._
import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.time.ZonedDateTime

object DataReader {

  object AnalysisDataEndpointJSONReader extends DefaultJsonProtocol {

    implicit object locationFormat
        extends RootJsonFormat[RequestLocation.Location] {
      override def read(json: JsValue): Location = {
        json.asInstanceOf[JsString].value match {
          case "URL"    => URL
          case "Cookie" => Cookie
          case "Body"   => Body
          case "Mixed"  => Mixed
        }
      }

      override def write(obj: Location): JsValue = {
        obj match {
          case URL    => JsString("URL")
          case Cookie => JsString("Cookie")
          case Body   => JsString("Body")
          case Mixed  => JsString("Mixed")
        }
      }
    }

    implicit object ZonedDateTimeFormat extends RootJsonFormat[ZonedDateTime] {
      override def read(json: JsValue): ZonedDateTime =
        throw new RuntimeException("write for zoned date time is not supported")

      override def write(obj: ZonedDateTime): JsValue = {
        JsString(obj.toString)
      }
    }

    implicit val headerFormat
      : RootJsonFormat[de.tubs.ias.ledeco.database.entities.Header] =
      jsonFormat2(de.tubs.ias.ledeco.database.entities.Header)

    implicit val cookieFormat
      : RootJsonFormat[de.tubs.ias.ledeco.database.entities.Cookie] =
      jsonFormat2(de.tubs.ias.ledeco.database.entities.Cookie)

    implicit object RequestFormat extends RootJsonFormat[Request] {
      override def read(json: JsValue): Request =
        throw new RuntimeException("read is not supported")

      override def write(obj: Request): JsValue = {
        JsObject(
          ("start", obj.start.toJson),
          ("method", JsString(obj.method)),
          ("host", JsString(obj.host)),
          ("pathAndParameter", JsString(obj.pathAndParameter)),
          ("content", JsString(obj.content)),
          ("port", JsString(obj.port)),
          ("scheme", JsString(obj.scheme)),
          ("authority", JsString(obj.authority)),
          ("http_version", JsString(obj.http_version)),
          ("cookies", JsArray(obj.cookies.map(_.toJson))),
          ("header", JsArray(obj.headers.map(_.toJson))),
        )
      }
    }

    implicit val patternFormat: RootJsonFormat[Pattern] =
      jsonFormat2(Pattern)

    implicit val exodusTrackerFormat: RootJsonFormat[ExodusTracker] =
      jsonFormat4(ExodusTracker)

    implicit val chatterFormat: RootJsonFormat[Chatter] = jsonFormat3(
      Chatter.apply
    )

    implicit object HoneyDataFormat extends RootJsonFormat[HoneyData] {
      override def read(json: JsValue): HoneyData = {
        val fields = json.asJsObject.fields
        HoneyData(
          fields("location").asInstanceOf[JsString].value,
          fields("values")
            .asInstanceOf[JsArray]
            .elements
            .map(_.asInstanceOf[JsString].value)
            .toList
        )
      }

      override def write(obj: HoneyData): JsValue = {
        JsObject(
          ("location", JsString(obj.location)),
          ("values", JsArray(obj.values.map(_.toJson)))
        )
      }
    }

    implicit object identifiedTrackerFormat
        extends RootJsonFormat[IdentifiedTracker] {
      override def read(json: JsValue): IdentifiedTracker = {
        val map = json.asJsObject.fields
        IdentifiedTracker(
          map("domain").asInstanceOf[JsString].value.split(",").toList,
          locationFormat.read(map("location")),
          map("name")
            .asInstanceOf[JsString]
            .value
            .split(",")
            .toList
            .filterNot(_ == "")
        )
      }

      override def write(obj: IdentifiedTracker): JsValue = {
        new JsObject(
          Map("domain" -> JsString(obj.domains.mkString(",")),
              "location" -> locationFormat.write(obj.location),
              "name" -> JsString(obj.names.mkString(","))))
      }
    }

    implicit object AnalysisDataFormat extends RootJsonFormat[AnalysisData] {
      override def read(json: JsValue): AnalysisData =
        throw new RuntimeException("read for analysis data is not supported")

      override def write(obj: AnalysisData): JsValue = {
        obj match {
          case x: Chatter           => x.toJson
          case x: HoneyData         => x.toJson
          case x: IdentifiedTracker => x.toJson
          case x: Pattern           => x.toJson
          case _                    => throw new RuntimeException("unknown analysis data type")
        }
      }
    }

    implicit val analysisResultMatchFormat
      : RootJsonFormat[AnalysisResultMatch] = jsonFormat3(AnalysisResultMatch)

  }

  import de.tubs.ias.ledeco.utility.DataReader.AnalysisDataEndpointJSONReader._

  def readFile(path: String,
               encoding: Charset = StandardCharset.UTF_8): String = {
    val encoded = Files.readAllBytes(Paths.get(path))
    new String(encoded, encoding)
  }

  def readInExodusTracker(path: String): Seq[ExodusTracker] = {
    JsonParser(readFile(path)).asJsObject
      .fields("trackers")
      .asInstanceOf[JsArray]
      .elements
      .map(_.convertTo[ExodusTracker])
  }

  def readInChatter(path: String): Seq[Chatter] = {
    JsonParser(readFile(path))
      .asInstanceOf[JsArray]
      .elements
      .map(_.convertTo[Chatter])
  }

  def readInHoneyData(path: String): Seq[HoneyData] = {
    JsonParser(readFile(path))
      .asInstanceOf[JsArray]
      .elements
      .map(_.convertTo[HoneyData])
  }

  def readInPatterns(path: String): Seq[Pattern] = {
    JsonParser(readFile(path))
      .asInstanceOf[JsArray]
      .elements
      .map(_.convertTo[Pattern])
  }

  def readInTracker(folder: String): Seq[IdentifiedTracker] = {
    new File(folder).listFiles().filter(_.isFile).map { file =>
      JsonParser(readFile(file.getAbsolutePath)).convertTo[IdentifiedTracker]
    }
  }

  def readInProcessedHashes(path: String): Seq[Int] = {
    assert(path.endsWith("_hashes.json"),
           s"file $path does not end on _hashes.json")
    JsonParser(readFile(path)).convertTo[JsArray].elements.map(_.convertTo[Int])
  }

}
