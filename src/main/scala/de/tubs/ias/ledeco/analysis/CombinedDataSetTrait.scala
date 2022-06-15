package de.tubs.ias.ledeco.analysis

import de.halcony.ppm.colors.Color
import de.halcony.ppm.graph.Coordinate
import de.halcony.ppm.graph.visual.bar.BarPlot

trait CombinedDataSetTrait extends BarGraphPlottable {

  def getLabel: String

  def getLabeledMonitoringAppsName(privacyType: Option[String],
                                   purpose: Option[String],
                                   category: Option[String],
                                   dataType: Option[String]): Seq[String]

  def getAppInMonitoringCount: Int

  def getAppsInMonitoringWithoutLabel: Seq[String]

  def getAppsInMonitoringBadApp: Seq[String]

  def getDetectedTrackerContactAppMapping(
      name: String): Map[String, List[AnalysisData]]

  def plotTrackerAnalysis(name: String, outFolder: String, top: Int): Unit

  def analysisHoneyDataTransmission(outFolder: String): Unit

  def plotHoneyDataTransmission(outFolder: String): Unit

  protected def mapToPlot(map: Map[String, Int],
                          fill: Color,
                          line: Color,
                          label: Option[String] = None): BarPlot = {
    val coordinates = map.toList.sortBy(_._2).reverse.map {
      case (name, value) => Coordinate(value, name)
    }
    assert(coordinates.nonEmpty,
           s"there are no coordinates generated for $label out of $map")
    val ret =
      new BarPlot().setLineColor(line).setFillColor(fill).addData(coordinates)
    label match {
      case Some(value) => ret.setName(value).asInstanceOf[BarPlot]
      case None        => ret.asInstanceOf[BarPlot]
    }
  }

  def getTrackerListNames: Set[String]

  def plotPotentialAppleIdentifierTransmission(outFolder: String): Unit

  def printTrackerNumbers(tracker: String): Unit

  def printDrainTargetHosts(): Unit

  def plotDrainTargetHosts(outFolder: String): Unit

  def plotIDFATransmission(outFolder: String): Unit

  def printIDFANumbers(): Unit

  def plotAppsThatShouldVsHavePrivacyNotice(outFolder: String,
                                            privacyNoticeFile: String): Unit

  def printAppsThatShouldVsHavePrivacyNoticeNumbers(
      privacyNoticeFile: String): Unit

  def printHoneyDataNumbers(): Unit

  def printBasicNumbers(): Unit

  def printNumberLabelViolation(): Unit

}
