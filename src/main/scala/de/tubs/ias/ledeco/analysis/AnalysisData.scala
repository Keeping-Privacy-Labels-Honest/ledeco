package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.database.entities.Request

trait AnalysisData {
  def isContained(request: Request): Option[AnalysisResultMatch]

  def isContained(request: Seq[Request]): Seq[AnalysisResultMatch] = {
    request.map(isContained).filter(_.nonEmpty).map(_.get)
  }

  def getLabel: String
}
