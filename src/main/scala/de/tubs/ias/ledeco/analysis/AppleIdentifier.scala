package de.tubs.ias.ledeco.analysis
import de.tubs.ias.ledeco.database.entities.Request
import de.tubs.ias.ledeco.utility.RequestLocation
import de.tubs.ias.ledeco.utility.RequestLocation.Cookie

case class AppleIdentifier(regexp: String, name: String) extends AnalysisData {

  override def isContained(request: Request): Option[AnalysisResultMatch] = {
    val cookiesp = request.cookies.exists(cookie =>
      cookie.values.contains(regexp.r.unanchored.matches(_)))
    val parameterp = regexp.r.unanchored.matches(request.pathAndParameter)
    // the honey data is contained in the content/body of the request
    val contentp = regexp.r.unanchored.matches(request.content)
    val headerp = request.headers.exists(header =>
      regexp.r.unanchored.matches(header.values))
    if (cookiesp) {
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
  }

  override def getLabel: String = name
}

//post 14.5
object IDFA
    extends AppleIdentifier("00000000-0000-0000-0000-000000000000", "IDFA")

object IDVA
    extends AppleIdentifier(
      "[a-zA-Z0-9]{8}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{12}",
      "IDVA") {

  override def isContained(request: Request): Option[AnalysisResultMatch] = {
    val result = super.isContained(request)
    result match {
      case Some(result) =>
        IDFA.isContained(result.request) match {
          case Some(_) => None
          case None    => Some(result)
        }
      case None => None
    }
  }

}
