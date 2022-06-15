package de.tubs.ias.ledeco.tracker

import de.tubs.ias.ledeco.analysis.{AnalysisData, AnalysisResultMatch}
import de.tubs.ias.ledeco.database.entities.Request
import de.tubs.ias.ledeco.utility.RequestLocation._

case class IdentifiedTracker(domains: List[String],
                             location: Location,
                             names: List[String])
    extends AnalysisData {

  val hosts: List[String] = domains.map { domain =>
    if (domain.startsWith("https://")) {
      domain.substring("https://".length)
    } else if (domain.startsWith("http://")) {
      domain.substring("http://".length)
    } else {
      domain
    }
  }

  val tld: String = {
    val tlds = domains.map { domain =>
      domain.split("\\.").reverse.slice(0, 2).reverse.mkString(".")
    }.toSet
    assert(tlds.toList.length == 1)
    tlds.head
  }

  private def checkDomain(req: Request): Boolean = {
    // checking for ends with as this will also include subdomains
    hosts.exists(req.host.endsWith)
  }

  override def getLabel: String = tld

  def checkUrl(req: Request): Boolean = {
    names.exists(req.getParameters.url.contains(_))
  }

  def checkCookie(req: Request): Boolean = {
    names.exists(req.getCookieMap.contains(_))
  }

  def checkHeader(req: Request): Boolean = {
    names.exists(req.getHeaderMap.contains(_))
  }

  override def isContained(req: Request): Option[AnalysisResultMatch] = {
    (checkDomain(req),
     checkUrl(req),
     checkCookie(req),
     checkHeader(req),
     location) match {
      case (_, _, _, _, Body) =>
        throw new RuntimeException("body in identified tracker not supported")
      case (true, true, _, _, x) if x == URL || x == Mixed =>
        Some(AnalysisResultMatch(req, this, URL))
      case (true, _, true, _, x) if x == Cookie || x == Mixed =>
        Some(AnalysisResultMatch(req, this, Cookie))
      case (true, _, _, true, x) if x == Header || x == Mixed =>
        Some(AnalysisResultMatch(req, this, Header))
      // if the domain matches and we have no name anyways
      case (true, _, _, _, _) if names.isEmpty =>
        Some(AnalysisResultMatch(req, this, URL))
      // if the domain matches but non of the tracker values exist
      case (true, false, false, false, _) if names.nonEmpty => None
      // if the domain does not match
      case (false, _, _, _, _) => None
      case x                   => throw new RuntimeException(s"unexpected isContained state $x")
    }
  }

  override def isContained(req: Seq[Request]): Seq[AnalysisResultMatch] = {
    req.map(isContained).filter(_.nonEmpty).map(_.get)
  }
}
