package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.utility.{DualColorWheel, Statistics}
import wvlet.log.LogSupport

class AggregatedCombinedDataSet(dataSets: Seq[CombinedDataSet])
    extends CombinedDataSetTrait
    with LogSupport {

  val merge: CombinedDataSet = dataSets.head.merge(dataSets.tail)

  override def getLabel: String = s"${dataSets.map(_.getLabel).mkString("_")}"

  override def getAppInMonitoringCount: Int = merge.getAppInMonitoringCount

  override def getLabeledMonitoringAppsName(
      privacyType: Option[String],
      purpose: Option[String],
      category: Option[String],
      dataType: Option[String]): Seq[String] = {
    merge.getLabeledMonitoringAppsName(privacyType, purpose, category, dataType)
  }

  override def getAppsInMonitoringWithoutLabel: Seq[String] =
    merge.getAppsInMonitoringWithoutLabel

  override def getAppsInMonitoringBadApp: Seq[String] =
    merge.getAppsInMonitoringBadApp

  override def analysisHoneyDataTransmission(outFolder: String): Unit =
    merge.analysisHoneyDataTransmission(outFolder)

  override def getDetectedTrackerContactAppMapping(
      tracker: String): Map[String, List[AnalysisData]] =
    merge.getDetectedTrackerContactAppMapping(tracker)

  override def plotTrackerAnalysis(tracker: String,
                                   out: String,
                                   top: Int): Unit = {
    val sorted =
      merge.getTrackerResults(tracker).toList.sortBy(_._2.length).reverse
    val actualTop = sorted.slice(0, List(sorted.length, top).min).map(_._1)
    dataSets.grouped(8).foreach { group =>
      val colorWheel = new DualColorWheel()
      val plots = group.map { elem =>
        val (fill, line) = colorWheel.getNextColors
        mapToPlot(elem
                    .getTrackerResults(tracker)
                    .filter(elem => actualTop.contains(elem._1))
                    .map(elem => elem._1 -> elem._2.length),
                  fill,
                  line,
                  Some(elem.getLabel))
      }
      val outFile =
        s"$out/${tracker}Contacts${group.map(_.getLabel).mkString("_")}"
      createBarGraph(outFile, plots, group.map(_.getAppInMonitoringCount).max)
    }
    merge.plotTrackerAnalysis(tracker, out, top)
  }

  def plotAppsThatShouldVsHavePrivacyNotice(outFolder: String,
                                            privacyNoticeFile: String): Unit = {
    merge.plotAppsThatShouldVsHavePrivacyNotice(outFolder, privacyNoticeFile)
    val colorWheel = new DualColorWheel()
    val plots = dataSets.map { ds =>
      val (fill, line) = colorWheel.getNextColors
      ds.getPlotAppsThatShouldVsHavePrivacyNotice(privacyNoticeFile, fill, line)
    }
    val outFile = s"$outFolder/GDPRRequiredAggregated"
    createBarGraph(outFile, plots, 1600)
  }

  override def printHoneyDataNumbers(): Unit = {
    merge.printHoneyDataNumbers()
  }

  override def printAppsThatShouldVsHavePrivacyNoticeNumbers(
      privacyNoticeFile: String): Unit = {
    merge.printAppsThatShouldVsHavePrivacyNoticeNumbers(privacyNoticeFile)
    val analysisTransmitting = dataSets.map(
      elem =>
        elem.getLabel -> (elem
          .getAppsTransmittingHoneyData() ++ elem.getIDFATransmission))
    val analysisCollecting =
      dataSets.map(elem => elem.getLabel -> elem.getAppsByLabelGDPRRequired)
    val having =
      CombinedDataSet.readInAppsHavingPrivacyNotice(privacyNoticeFile)
    val analysisHaving = dataSets.map(elem =>
      elem.getLabel -> elem.getMonitoredIphoneAppNames.intersect(having))
    val analysisTransmittingYetNoDialogue =
      (analysisTransmitting zip analysisHaving).map {
        case ((transmittingLabel, transmittingNames),
              (havingLabel, havingNames)) =>
          assert(transmittingLabel == havingLabel)
          transmittingLabel -> transmittingNames.diff(havingNames)
      }
    val analysisCollectingYetNoDialogue =
      (analysisCollecting zip analysisHaving).map {
        case ((collectingLabel, collectingNames), (havingLabel, havingNames)) =>
          assert(collectingLabel == havingLabel)
          collectingLabel -> collectingNames.diff(havingNames)
      }
    val transmittingMost = analysisTransmitting.maxBy(_._2.size)
    println(
      s"transmitting most: ${transmittingMost._1} / ${transmittingMost._2.size}")
    val (transmittingAvg, transmittingSigma) =
      Statistics.statisticalAnalysis(analysisTransmitting.map(_._2.size))
    println(s"transmitting avg: $transmittingAvg")
    println(s"transmitting sigma: $transmittingSigma")
    val collectingMost = analysisCollecting.maxBy(_._2.size)
    println(
      s"collecting most: ${collectingMost._1} / ${collectingMost._2.size}")
    val (collectingavg, collectingsigma) =
      Statistics.statisticalAnalysis(analysisCollecting.map(_._2.size))
    println(s"collecting avg: $collectingavg")
    println(s"collecting sigma: $collectingsigma")
    val havingMost = analysisHaving.maxBy(_._2.size)
    println(s"having most: ${havingMost._1} / ${havingMost._2.size}")
    val (havingAvg, havingSigma) =
      Statistics.statisticalAnalysis(analysisHaving.map(_._2.size))
    println(s"having avg: $havingAvg")
    println(s"having sigma: $havingSigma")
    val missingDialogueTransmittingMost =
      analysisTransmittingYetNoDialogue.maxBy(_._2.size)
    println(
      s"missing dialogue transmitting most: ${missingDialogueTransmittingMost._1} / ${missingDialogueTransmittingMost._2.size}")
    val (missingDialogueTransmittingAvg, missingDialogueTransmittingSigma) =
      Statistics.statisticalAnalysis(
        analysisTransmittingYetNoDialogue.map(_._2.size))
    println(
      s"missing dialogue transmitting avg: $missingDialogueTransmittingAvg")
    println(
      s"missing dialogue transmitting sigma: $missingDialogueTransmittingSigma")
    val missingDialogueCollectingMost =
      analysisCollectingYetNoDialogue.maxBy(_._2.size)
    println(
      s"missing dialogue collecting most: ${missingDialogueCollectingMost._1} / ${missingDialogueCollectingMost._2.size}")
    val (missingDialogueCollectinAvg, missingDialogueCollectinSigma) =
      Statistics.statisticalAnalysis(
        analysisCollectingYetNoDialogue.map(_._2.size))
    println(s"missing dialogue collecting avg: $missingDialogueCollectinAvg")
    println(s"missing dialogue collecting avg: $missingDialogueCollectinSigma")
  }

  override def plotHoneyDataTransmission(outFolder: String): Unit = {
    merge.plotHoneyDataTransmission(outFolder)
  }

  override def plotPotentialAppleIdentifierTransmission(
      outFolder: String): Unit = {
    merge.plotHoneyDataTransmission(outFolder)
  }

  override def printTrackerNumbers(tracker: String): Unit = {
    merge.printTrackerNumbers(tracker)
    // how many apps contact at least one tracker
    val analysisData =
      this.dataSets.map(elem =>
        (elem.getLabel, elem.getContactedTrackerCount(tracker)))
    val least = analysisData.minBy(_._2)
    val most = analysisData.maxBy(_._2)
    val (avg, sigma) = Statistics.statisticalAnalysis(analysisData.map(_._2))
    // what is the most popular tracking domain
    val mostPopularTracker = merge.getTrackerCounts(tracker).maxBy(_._2)
    val (mostPopularTrackerAvg, mostPopularTrackerSigma) =
      Statistics.statisticalAnalysis(
        dataSets
          .map(_.getTrackerCounts(tracker))
          .map(_.getOrElse(mostPopularTracker._1, 0))
      )
    println(s"Most: ${most._1} / ${most._2}")
    println(s"Least: ${least._1} / ${least._2}")
    println(s"Avg: $avg")
    println(s"Sigma: $sigma")
    println(
      s"Most Tracker: ${mostPopularTracker._1} / ${mostPopularTracker._2}")
    println(s"Most Tracker Avg: $mostPopularTrackerAvg")
    println(s"Most Tracker Avg: $mostPopularTrackerSigma")
  }

  override def printDrainTargetHosts(): Unit = {
    merge.printDrainTargetHosts()
  }

  override def plotDrainTargetHosts(outFolder: String): Unit = {
    merge.plotDrainTargetHosts(outFolder)
  }

  override def printIDFANumbers(): Unit = {
    merge.printIDFANumbers()
    val analysis =
      dataSets.map(elem => (elem.getLabel, elem.getIDFATransmission.size))
    val max = analysis.maxBy(_._2)
    val (avg, sigma) = Statistics.statisticalAnalysis(analysis.map(_._2))
    println(s"Most IDFA transmissions: ${max._1} / ${max._2}")
    println(s"IDFA Avg: $avg")
    println(s"IDFA Sigma $sigma")
  }

  override def plotIDFATransmission(outFolder: String): Unit = {
    merge.plotIDFATransmission(outFolder)
  }

  override def printBasicNumbers(): Unit = {
    merge.printBasicNumbers()
    val analysis = dataSets.map(elem => elem.getLabel -> elem.getRequestData._1)
    val max = analysis.maxBy(_._2)
    val (avg, sigma) = Statistics.statisticalAnalysis(analysis.map(_._2))
    println(s"requests max     : ${max._1} / ${max._1}")
    println(s"request avg      : $avg")
    println(s"request sigma    : $sigma")
  }

  override def printNumberLabelViolation(): Unit = {
    merge.printNumberLabelViolation()
    val analysis =
      dataSets.map(elem => elem.getLabel -> elem.getAppsLabelViolation)
    val max = analysis.maxBy(_._2.size)
    val (avg, sigm) = Statistics.statisticalAnalysis(analysis.map(_._2.size))
    println(s"max label violations: ${max._1} / ${max._2.size}")
    println(s"avg label violations: $avg")
    println(s"sigma label violations: $sigm")
  }

  override def getTrackerListNames: Set[String] = merge.getTrackerListNames
}
