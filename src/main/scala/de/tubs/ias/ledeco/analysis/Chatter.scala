package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.database.entities.{Collection, Request}
import de.tubs.ias.ledeco.utility.RequestLocation.URL
import wvlet.log.LogSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

case class Chatter(hostRegexp: String, usage: String, source: String)
    extends AnalysisData {

  override def getLabel: String = s"Chatter $hostRegexp $usage $source"

  override def isContained(request: Request): Option[AnalysisResultMatch] = {
    if (request.host.endsWith(hostRegexp)) {
      Some(AnalysisResultMatch(request, this, URL))
    } else {
      None
    }
  }

  override def isContained(request: Seq[Request]): Seq[AnalysisResultMatch] = {
    throw new RuntimeException(
      "chatter is request specific - must not apply on list of requests")
  }
}

object Chatter extends LogSupport {

  def purgeChatter(collection: Collection,
                   chatter: Seq[Chatter]): Collection = {
    info(
      s"purging ${chatter.size} chatter data points in ${collection.monitoring.size} apps")
    val future = Future.sequence {
      collection.monitoring.map {
        case (app, requests) =>
          Future {
            app -> requests.filterNot(request =>
              chatter.exists(_.isContained(request).nonEmpty))
          }
      }
    }
    val purged = Await.result(future, Inf)
    info(s"after purge we stil have ${purged.size} apps")
    Collection(collection.id,
               collection.phone,
               collection.start,
               collection.end,
               purged)
  }

}
