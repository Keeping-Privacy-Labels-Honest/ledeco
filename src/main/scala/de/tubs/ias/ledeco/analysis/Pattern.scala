package de.tubs.ias.ledeco.analysis
import de.tubs.ias.ledeco.database.entities.Request
import de.tubs.ias.ledeco.utility.RequestLocation
import de.tubs.ias.ledeco.utility.RequestLocation._

case class Pattern(description: String, regexp: String) extends AnalysisData {

  override def isContained(request: Request): Option[AnalysisResultMatch] = {
    val cookiesp = request.cookies.exists { cookie =>
      regexp.r.unanchored.matches(cookie.values)
    }
    val pathAndParameterp =
      regexp.r.unanchored.matches(request.pathAndParameter)
    val contentp = regexp.r.unanchored.matches(request.content)
    val headerp = request.headers.exists(header =>
      regexp.r.unanchored.matches(header.values))
    if (cookiesp) {
      Some(AnalysisResultMatch(request, this, Cookie))
    } else if (pathAndParameterp) {
      Some(AnalysisResultMatch(request, this, RequestLocation.URL))
    } else if (contentp) {
      Some(AnalysisResultMatch(request, this, RequestLocation.Body))
    } else if (headerp) {
      Some(AnalysisResultMatch(request, this, RequestLocation.Header))
    } else {
      None
    }
  }

  override def getLabel: String = description

}
