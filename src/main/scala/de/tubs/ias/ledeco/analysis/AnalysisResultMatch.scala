package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.database.entities.Request
import de.tubs.ias.ledeco.utility.RequestLocation

case class AnalysisResultMatch(request: Request,
                               analysisData: AnalysisData,
                               location: RequestLocation.Location) {

  override def toString: String = {
    s"$analysisData -> $request"
  }

}
