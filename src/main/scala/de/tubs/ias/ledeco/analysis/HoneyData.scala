package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.database.entities.Request
import de.tubs.ias.ledeco.utility.RequestLocation
import de.tubs.ias.ledeco.utility.RequestLocation.Cookie
import wvlet.log.LogSupport

case class HoneyData(location: String, values: List[String])
    extends AnalysisData
    with LogSupport {

  override def getLabel: String = location

  override def isContained(request: Request): Option[AnalysisResultMatch] = {
    // either the honey data is a cookie value
    val cookiesp = request.cookies.exists(cookie =>
      this.values.exists(honeyValue => cookie.values.contains(honeyValue)))
    // the honey data is contained in a path or parameter
    val parameterp = this.values.exists(request.pathAndParameter.contains(_))
    // the honey data is contained in the content/body of the request
    val contentp = this.values.exists { value =>
      if (request.content.contains(value)) {
        true
      } else {
        false
      }
    }
    val headerp = request.headers.exists(header =>
      this.values.exists(honeyValue => header.values.contains(honeyValue)))
    val res = if (cookiesp) {
      Some(AnalysisResultMatch(request, this, Cookie))
    } else if (parameterp) {
      Some(AnalysisResultMatch(request, this, RequestLocation.URL))
    } else if (contentp) {
      Some(AnalysisResultMatch(request, this, RequestLocation.Body))
    } else if (headerp) {
      Some(AnalysisResultMatch(request, this, RequestLocation.Header))
    } else {
      None
    }
    res match {
      case Some(_) =>
      case None    =>
    }
    res
  }

}
