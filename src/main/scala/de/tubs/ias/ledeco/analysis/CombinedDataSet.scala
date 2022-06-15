package de.tubs.ias.ledeco.analysis

import de.halcony.ppm.colors.{Black, Color, Red}
import de.halcony.ppm.graph.Coordinate
import de.halcony.ppm.graph.visual.bar.BarPlot
import de.tubs.ias.ledeco.analysis.CombinedDataSet.readInAppsHavingPrivacyNotice
import de.tubs.ias.ledeco.database.entities.Collection
import de.tubs.ias.ledeco.privacyLabels.elements._
import de.tubs.ias.ledeco.utility.Statistics
import wvlet.log.LogSupport

import java.io.{File, FileWriter}
import scala.collection.mutable.{Map => MMap}
import scala.io.Source

sealed trait CollectionType
object Iphone extends CollectionType
object Android extends CollectionType

class CombinedDataSet(label: String,
                      iphoneCollection: Collection,
                      translateIdName: Set[(String, String)],
                      privacyLabelAnalysis: PrivacyLabelAnalysis,
                      tracker: Map[String, List[AnalysisData]],
                      honey: Seq[AnalysisData],
                      honeyDataMapping: Map[String, List[String]],
                      pattern: Seq[AnalysisData])
    extends LogSupport
    with CombinedDataSetTrait {

  override def getTrackerListNames: Set[String] = tracker.keySet

  def getTranslateIdNames: Set[(String, String)] = translateIdName

  private val appNameToId: Map[String, String] =
    translateIdName.map(elem => elem._2 -> elem._1).toMap

  private val appIdToName: Map[String, String] = translateIdName.toMap

  private def getAppId(appName: String) =
    appNameToId.getOrElse(
      appName,
      throw new RuntimeException(s"We do not know the ID for App $appName"))

  private def getAppName(appId: String) =
    appIdToName.getOrElse(
      appId,
      throw new RuntimeException(s"We do not know the Name for ID $appId"))

  private val monitoredIphoneAppNames: Set[String] = {
    iphoneCollection.monitoring.map(_._1.name).toSet
  }

  private val monitoredIphoneAppIds: Set[String] =
    monitoredIphoneAppNames.map(getAppId)

  val filteredMap: Map[String, Option[PrivacyDetails]] =
    this.privacyLabelAnalysis.privacyLabelSet.filter(elem =>
      monitoredIphoneAppIds.contains(elem._1))

  def getPrivacyLabelAnalysis: PrivacyLabelAnalysis = privacyLabelAnalysis

  def merge(ds: Seq[CombinedDataSet]): CombinedDataSet = {
    val newIphoneCollection =
      iphoneCollection.merge(ds.map(_.getIphoneCollection))
    val newPrivacyLabelAnalysis =
      privacyLabelAnalysis.merge(ds.map(_.getPrivacyLabelAnalysis))
    val newTranslatedName: Set[(String, String)] = translateIdName ++ ds
      .flatMap(_.getTranslateIdNames)
    val newLabel = s"merged"
    new CombinedDataSet(
      newLabel,
      newIphoneCollection,
      newTranslatedName,
      newPrivacyLabelAnalysis,
      tracker,
      honey,
      honeyDataMapping,
      pattern
    )
  }

  override def getLabel: String = label

  def getIphoneCollection: Collection = iphoneCollection

  private val trackerAnalysisIphone = tracker.map {
    case (name, data) =>
      assert(data.nonEmpty,
             s"the given tracker set $name does not contain any tracker")
      name -> new Analysis(iphoneCollection, data).run()
  }
  private val honeyAnalysisIphone =
    new Analysis(iphoneCollection, honey).run()
  private val IDFAAnalysis =
    new Analysis(iphoneCollection, List(IDFA)).run()
  private val IDVAAnalysis =
    new Analysis(iphoneCollection, List(IDVA)).run()

  def getPrivacyLabels: PrivacyLabelAnalysis = privacyLabelAnalysis

  override def getAppInMonitoringCount: Int = iphoneCollection.monitoring.length

  override def getLabeledMonitoringAppsName(
      privacyType: Option[String],
      purpose: Option[String],
      category: Option[String],
      dataType: Option[String]): Seq[String] = {
    filteredMap
      .filter(_._2.nonEmpty)
      .filter(_._2.get.exists(privacyType, purpose, category, dataType))
      .keys
      .toSeq
      .map(getAppName)
  }

  override def getAppsInMonitoringWithoutLabel: Seq[String] = {
    filteredMap
      .filter(_._2.nonEmpty)
      .filter(_._2.get.privacyTypes.isEmpty)
      .keys
      .toSeq
      .map(getAppName)
  }

  override def getAppsInMonitoringBadApp: Seq[String] = {
    filteredMap.filter(_._2.isEmpty).keys.toSeq.map(getAppName)
  }

  override def getDetectedTrackerContactAppMapping(
      tracker: String): Map[String, List[AnalysisData]] = {
    trackerAnalysisIphone
      .getOrElse(
        tracker,
        throw new RuntimeException(s"the tracker $tracker does not exist"))
      .getAppToTrackers
      .filter(_._2.nonEmpty)
      .map(elem => elem._1.name -> elem._2)
  }

  def getTrackerResults(tracker: String): Map[String, List[String]] = {
    trackerAnalysisIphone
      .getOrElse(
        tracker,
        throw new RuntimeException(s"the tracker $tracker does not exist"))
      .getTrackerApps
      .map(elem => elem._1.getLabel -> elem._2.map(_.name))
  }

  def getContactedTrackerCount(tracker: String): Int = {
    trackerAnalysisIphone
      .getOrElse(
        tracker,
        throw new RuntimeException(s"the tracker $tracker does not exist"))
      .getAppToTrackers
      .count(_._2.nonEmpty)
  }

  def getTrackerCounts(tracker: String): Map[String, Int] = {
    trackerAnalysisIphone
      .getOrElse(
        tracker,
        throw new RuntimeException(s"the tracker $tracker does not exist"))
      .getTrackerApps
      .map(elem => (elem._1.getLabel, elem._2.length))
  }

  private def mergeHostCounts(
      analysisHosts: Map[AnalysisData, Map[String, Int]]): Map[String, Int] = {
    val ret: MMap[String, Int] = MMap()
    analysisHosts.foreach {
      case (_, value) =>
        value.foreach {
          case (host, count) =>
            if (ret.contains(host)) {
              ret(host) += count
            } else {
              ret.addOne(host -> count)
            }
        }
    }
    ret.toMap
  }

  override def printDrainTargetHosts(): Unit = {
    val combined: Map[AnalysisData, Map[String, Int]] = IDFAAnalysis.getHosts(
      Set()) ++ honeyAnalysisIphone.getHosts(Set("os-version"))
    combined.foreach {
      case (ad, map) =>
        println()
        println(s"Analysis Data: ${ad.getLabel}")
        map.toList.sortBy(_._2).foreach {
          case (host, count) => println(s"$host : $count")
        }
    }
    println()
    println("Combined Honey Data")
    mergeHostCounts(honeyAnalysisIphone.getHosts(Set("os-version"))).toList
      .sortBy(_._2)
      .foreach {
        case (host, count) => println(s"$host : $count")
      }
    println()
    println("Combined all")
    mergeHostCounts(combined).toList.sortBy(_._2).foreach {
      case (host, count) => println(s"$host : $count")
    }
  }

  override def plotDrainTargetHosts(outFolder: String): Unit = {
    val combined: Map[AnalysisData, Map[String, Int]] = IDFAAnalysis.getHosts(
      Set()) ++ honeyAnalysisIphone.getHosts(Set("os-version"))
    combined.foreach {
      case (ad, map) =>
        createBarGraph(s"$outFolder/HostTargets${ad.getLabel}",
                       List(mapToPlot(map, Black, Black, Some(ad.getLabel))),
                       42)
    }
    createBarGraph(
      s"$outFolder/HostTargetsHoneyData",
      List(
        mapToPlot(
          mergeHostCounts(honeyAnalysisIphone.getHosts(Set("os-version"))),
          Black,
          Black,
          Some("Honey Data"))),
      42
    )
    createBarGraph(
      s"$outFolder/HostTargetsCombined",
      List(
        mapToPlot(mergeHostCounts(combined), Black, Black, Some("Combined"))),
      42)
  }

  override def printTrackerNumbers(tracker: String): Unit = {
    val analysis = trackerAnalysisIphone.getOrElse(
      tracker,
      throw new RuntimeException(s"the tracker $tracker does not exist"))
    val mostPopularTracker = analysis.getTrackerApps
      .map(elem => (elem._1.getLabel, elem._2.length))
      .maxBy(_._2)
    val leastPopularTracker = analysis.getTrackerApps
      .map(elem => (elem._1.getLabel, elem._2.length))
      .filter(_._2 > 0)
      .minBy(_._2)
    println(s"At Least One  : ${getContactedTrackerCount(tracker)}")
    println(
      s"Most  Popular: ${mostPopularTracker._1} / ${mostPopularTracker._2}")
    println(
      s"Least Popular: ${leastPopularTracker._1} / ${leastPopularTracker._2}")
  }

  def getAppsTransmittingHoneyData(
      except: Set[String] = Set("os-version")): Set[String] = {
    val filtered = honeyAnalysisIphone.getAppToTrackers
      .map(elem =>
        elem._1 -> elem._2.filterNot(elem => except.contains(elem.getLabel)))
      .filter(_._2.nonEmpty)
    filtered.map(_._1.name).toSet
  }

  override def printHoneyDataNumbers(): Unit = {
    honeyAnalysisIphone.getTrackerApps.foreach {
      case (data, value) => println(s"${data.getLabel} -> ${value.size}")
    }
  }

  def getAppsByLabelGDPRRequired: Set[String] = {
    val linked = this.getLabeledMonitoringAppsName(Some("DATA_LINKED_TO_YOU"),
                                                   None,
                                                   None,
                                                   None)
    val tracking = this.getLabeledMonitoringAppsName(
      Some("DATA_USED_TO_TRACK_YOU"),
      None,
      None,
      None)
    (linked ++ tracking).toSet
  }

  def getPlotAppsThatShouldVsHavePrivacyNotice(privacyNoticeFile: String,
                                               fill: Color = Black,
                                               line: Color = Black): BarPlot = {
    val transmitting = getAppsTransmittingHoneyData() ++ getIDFATransmission
    val collecting = getAppsByLabelGDPRRequired
    val having = readInAppsHavingPrivacyNotice(privacyNoticeFile).intersect(
      this.monitoredIphoneAppNames)
    val havingNoNoticeAndRequiringItTransmission = transmitting.diff(having)
    val havingNoNoticeAndRequiringItCollection = collecting.diff(having)
    val coordinates = List(
      Coordinate(transmitting.size.toString, "transmitting GDPR data"),
      Coordinate(collecting.size.toString, "collecting"),
      Coordinate(having.size.toString, "having notice"),
      Coordinate(havingNoNoticeAndRequiringItTransmission.size.toString,
                 "missing notice honey"),
      Coordinate(havingNoNoticeAndRequiringItCollection.size.toString,
                 "missing notice collection")
    )
    new BarPlot()
      .setLineColor(line)
      .setFillColor(fill)
      .addData(coordinates)
      .setName(this.getLabel)
      .asInstanceOf[BarPlot]
  }

  def getMonitoredIphoneAppNames: Set[String] = monitoredIphoneAppNames

  override def printAppsThatShouldVsHavePrivacyNoticeNumbers(
      privacyNoticeFile: String): Unit = {
    val transmitting = getAppsTransmittingHoneyData() ++ getIDFATransmission
    val collecting = getAppsByLabelGDPRRequired
    val having = readInAppsHavingPrivacyNotice(privacyNoticeFile).intersect(
      this.monitoredIphoneAppNames)
    val havingNoNoticeAndRequiringItTransmission = transmitting.diff(having)
    val havingNoNoticeAndRequiringItCollection = collecting.diff(having)
    println(s"transmitting: ${transmitting.size}")
    println(s"collecting  : ${collecting.size}")
    println(s"having notice: ${having.size}")
    println(
      s"required due to honey data: ${havingNoNoticeAndRequiringItTransmission.size}")
    println(
      s"required due to collection: ${havingNoNoticeAndRequiringItCollection.size}")
  }

  override def plotAppsThatShouldVsHavePrivacyNotice(
      outFolder: String,
      privacyNoticeFile: String): Unit = {
    val outFile = s"$outFolder/GDPRRequired"
    val plot = getPlotAppsThatShouldVsHavePrivacyNotice(privacyNoticeFile)
    createBarGraph(outFile, List(plot), this.monitoredIphoneAppNames.size)
  }

  type appLabelDistribution = (Set[String],
                               Set[String],
                               Set[String],
                               Set[String],
                               Set[String],
                               Set[String])
  def getDeclaringApps(dataCategory: String): appLabelDistribution = {
    val linked = this
      .getLabeledMonitoringAppsName(Some("DATA_LINKED_TO_YOU"),
                                    None,
                                    Some(dataCategory),
                                    None)
      .toSet
    val notLinked = this
      .getLabeledMonitoringAppsName(Some("DATA_NOT_LINKED_TO_YOU"),
                                    None,
                                    Some(dataCategory),
                                    None)
      .toSet
    val tracking = this
      .getLabeledMonitoringAppsName(Some("DATA_USED_TO_TRACK_YOU"),
                                    None,
                                    Some(dataCategory),
                                    None)
      .toSet
    val noDataCollected = this
      .getLabeledMonitoringAppsName(Some("DATA_NOT_COLLECTED"),
                                    None,
                                    None,
                                    None)
      .toSet
    val unknown = this.getAppsInMonitoringWithoutLabel.toSet
    val appsBad = this.getAppsInMonitoringBadApp.toSet
    (tracking, linked, notLinked, noDataCollected, unknown, appsBad)
  }

  def getHoneyDataPlots(honeyDataLabel: String,
                        apps: Seq[String]): Seq[BarPlot] = {
    val appsTransmitting = apps.toSet
    val inBetween = this.honeyDataMapping
      .getOrElse(
        honeyDataLabel,
        throw new RuntimeException(s"unknown honey data label $honeyDataLabel"))
      .map { honeyDataCategory =>
        getDeclaringApps(honeyDataCategory)
      }
    val (appsDeclaringTracking,
         appsDeclaringLinked,
         appsDeclaringNotLinked,
         appsDeclaringNoCollection,
         appsNotDeclaring,
         appsBad): appLabelDistribution =
      inBetween.reduce {
        (lhs: appLabelDistribution, rhs: appLabelDistribution) =>
          val (a, b, c, d, e, f) = lhs
          val (aa, bb, cc, dd, ee, ff) = rhs
          (a ++ aa, b ++ bb, c ++ cc, d ++ dd, e ++ ee, f ++ ff)
      }
    val allAppsDeclaringLabelOrMissing = appsDeclaringLinked ++ appsDeclaringLinked ++ appsDeclaringNotLinked ++ appsDeclaringNoCollection ++ appsNotDeclaring ++ appsBad
    val allAppsNotDeclaringLabelButNotMissing =
      monitoredIphoneAppNames.diff(allAppsDeclaringLabelOrMissing)
    val allAppsNotDeclaringLabelButNotMissingIntersec =
      appsTransmitting.intersect(allAppsNotDeclaringLabelButNotMissing)
    assert(
      (allAppsNotDeclaringLabelButNotMissing ++ allAppsDeclaringLabelOrMissing).size == monitoredIphoneAppNames.size)
    val appsDeclaringLinkedIntersec =
      appsTransmitting.intersect(appsDeclaringLinked)
    val appsDeclaringNotLinkedIntersec =
      appsTransmitting.intersect(appsDeclaringNotLinked)
    val appsDeclaringTrackingIntersec =
      appsTransmitting.intersect(appsDeclaringTracking)
    val appsUnknownIntersec = appsTransmitting.intersect(appsBad)
    val appsNotDeclaringIntersec = appsTransmitting.intersect(appsNotDeclaring)
    val appsDeclaringNoCollectionIntersec =
      appsTransmitting.intersect(appsDeclaringNoCollection)
    val theoretically = new BarPlot()
      .setColor(Black)
      .addData(
        List(
          Coordinate(appsDeclaringLinked.size.toString, "Linked"),
          Coordinate(appsDeclaringNotLinked.size.toString, "Not Linked"),
          Coordinate(appsDeclaringTracking.size.toString, "Tracking"),
          Coordinate(appsDeclaringNoCollection.size.toString, "No collection"),
          Coordinate(allAppsNotDeclaringLabelButNotMissing.size.toString,
                     "Not Declaring"),
          Coordinate(appsNotDeclaring.size.toString, "No Label"),
          Coordinate(appsBad.size.toString, "Unknown app"),
        ))
      .setName("theoretically")
      .asInstanceOf[BarPlot]
    val observation = new BarPlot()
      .setColor(Red)
      .addData(
        List(
          Coordinate(appsDeclaringLinkedIntersec.size.toString, "Linked"),
          Coordinate(appsDeclaringNotLinkedIntersec.size.toString,
                     "Not Linked"),
          Coordinate(appsDeclaringTrackingIntersec.size.toString, "Tracking"),
          Coordinate(appsDeclaringNoCollectionIntersec.size.toString,
                     "No collection"),
          Coordinate(
            allAppsNotDeclaringLabelButNotMissingIntersec.size.toString,
            "Not Declaring"),
          Coordinate(appsNotDeclaringIntersec.size.toString, "No Label"),
          Coordinate(appsUnknownIntersec.size.toString, "Unknown app"),
        ))
      .setName("observation")
      .asInstanceOf[BarPlot]
    List[BarPlot](theoretically, observation)
  }

  override def plotTrackerAnalysis(tracker: String,
                                   out: String,
                                   top: Int): Unit = {
    val sorted: Seq[(String, List[String])] =
      getTrackerResults(tracker).toList.sortBy(_._2.length).reverse
    assert(sorted.nonEmpty, "getTrackerResults returned empty list")
    val plot = mapToPlot(sorted
                           .slice(0, List(top, sorted.length).min)
                           .map(elem => elem._1 -> elem._2.length)
                           .toMap,
                         Black,
                         Black)
    val outFile = s"$out/${tracker}Contacts"
    createBarGraph(outFile, List(plot), this.getAppInMonitoringCount)
  }

  def getAppleIdentifierPlot(fill: Color, line: Color): BarPlot = {
    val coordinates = List(
      Coordinate(IDFAAnalysis.getTrackerApps.head._2.length.toString, "IDFA"),
      Coordinate(IDVAAnalysis.getTrackerApps.head._2.length.toString, "IDVA"),
    )
    new BarPlot()
      .setLineColor(line)
      .setFillColor(fill)
      .addData(coordinates)
      .setName(label)
      .asInstanceOf[BarPlot]
  }

  override def plotPotentialAppleIdentifierTransmission(
      outFolder: String): Unit = {
    val file = s"$outFolder/appleIdentifier"
    val plot = getAppleIdentifierPlot(Black, Black)
    createBarGraph(file, List(plot), this.getAppInMonitoringCount)
  }

  override def plotHoneyDataTransmission(outFolder: String): Unit = {
    honeyAnalysisIphone.getTrackerApps.foreach {
      case (honeyData, apps) =>
        val file = s"$outFolder/${honeyData.getLabel}"
        val plots = getHoneyDataPlots(honeyData.getLabel, apps.map(_.name))
        createBarGraph(file, plots, this.getAppInMonitoringCount)
    }
  }

  override def analysisHoneyDataTransmission(outFolder: String): Unit = {
    val fileWriter = new FileWriter(
      new File(s"$outFolder/honeyDataAnalysis.txt"))
    try {
      val allMatches = honeyAnalysisIphone.getAppToMatches.flatMap { elem =>
        elem._2
      }
      allMatches.groupBy(_.analysisData.getLabel).foreach {
        case (label, matches) =>
          fileWriter.write("\n")
          fileWriter.write(s"$label\n")
          matches.groupBy(_.request.host).foreach {
            case (str, value) => fileWriter.write(s"$str -> ${value.size}\n")
          }
      }
    } finally {
      fileWriter.flush()
      fileWriter.close()
    }
  }

  def getIDFATransmission: Set[String] = {
    this.IDFAAnalysis.getAppToTrackers
      .filter(_._2.nonEmpty)
      .map(_._1.name)
      .toSet
  }

  def getTrackingIdentifierLabelAppNames: (Set[String], Set[String]) = {
    this.privacyLabelAnalysis.privacyLabelSet
      .filter { elem =>
        elem._2 match {
          case Some(value) =>
            value.exists(Some("DATA_USED_TO_TRACK_YOU"),
                         None,
                         Some("IDENTIFIERS"),
                         None)
          case None => false
        }
      }
      .keySet
      .map { elem =>
        if (appIdToName.contains(elem)) {
          (List(appIdToName(elem)), List[String]())
        } else { (List[String](), List(elem)) }
      }
      .reduce { (lhs, rhs) =>
        (lhs._1 ++ rhs._1, lhs._2 ++ rhs._2)
      } match {
      case (lhs: List[String], rhs: List[String]) => (lhs.toSet, rhs.toSet)
    }
  }

  override def printIDFANumbers(): Unit = {
    println(s"IDFA Transmissions: ${getIDFATransmission.size}")
  }

  override def plotIDFATransmission(outFolder: String): Unit = {
    val observed = getIDFATransmission
    val (declared, _) = getTrackingIdentifierLabelAppNames
    val declaredIntersec = observed.intersect(declared)
    val unknown = getAppsInMonitoringBadApp.toSet
    val unknownIntersec = observed.intersect(unknown)
    val noLabel = getAppsInMonitoringWithoutLabel.toSet
    val noLabelIntersec = observed.intersect(noLabel)
    val noDataCollected = this
      .getLabeledMonitoringAppsName(Some("DATA_NOT_COLLECTED"),
                                    None,
                                    None,
                                    None)
      .toSet
    val noDataCollectedIntersec = observed.intersect(noDataCollected)
    val notDeclared = monitoredIphoneAppNames.filter(
      elem =>
        !unknown.contains(elem) && !declared.contains(elem) && !noLabel
          .contains(elem) && !noDataCollected.contains(elem))
    val notDeclaredIntersec = observed.intersect(notDeclared)
    val plots = List(
      new BarPlot()
        .setColor(Black)
        .addData(List(
          Coordinate(unknown.size.toString, "unknown"),
          Coordinate(noLabel.size.toString, "no label"),
          Coordinate(declared.size.toString, "declared"),
          Coordinate(notDeclared.size.toString, "not declared"),
          Coordinate(noDataCollected.size.toString, "no data collected")
        ))
        .setName("label"),
      new BarPlot()
        .setColor(Red)
        .addData(List(
          Coordinate(unknownIntersec.size.toString, "unknown"),
          Coordinate(noLabelIntersec.size.toString, "no label"),
          Coordinate(declaredIntersec.size.toString, "declared"),
          Coordinate(notDeclaredIntersec.size.toString, "not declared"),
          Coordinate(noDataCollectedIntersec.size.toString, "no data collected")
        ))
        .setName("observed")
    )
    val outFile = s"$outFolder/IDFATransmissions"
    createBarGraph(outFile, plots, getAppInMonitoringCount)
  }

  def appsViolatingLabelHoneyData(
      honeyDataLabel: String,
      appsTransmitting: Set[String]): Set[String] = {
    val inBetween = this.honeyDataMapping
      .getOrElse(
        honeyDataLabel,
        throw new RuntimeException(s"unknown honey data label $honeyDataLabel"))
      .map { honeyDataCategory =>
        getDeclaringApps(honeyDataCategory)
      }
    val (appsDeclaringTracking,
         appsDeclaringLinked,
         appsDeclaringNotLinked,
         appsDeclaringNoCollection,
         appsNotDeclaring,
         appsBad): appLabelDistribution =
      inBetween.reduce {
        (lhs: appLabelDistribution, rhs: appLabelDistribution) =>
          val (a, b, c, d, e, f) = lhs
          val (aa, bb, cc, dd, ee, ff) = rhs
          (a ++ aa, b ++ bb, c ++ cc, d ++ dd, e ++ ee, f ++ ff)
      }
    val allAppsDeclaringLabelOrMissing = appsDeclaringLinked ++ appsDeclaringLinked ++ appsDeclaringNotLinked ++ appsDeclaringNoCollection ++ appsNotDeclaring ++ appsBad
    val allAppsNotDeclaringLabelButNotMissing =
      monitoredIphoneAppNames.diff(allAppsDeclaringLabelOrMissing)
    appsTransmitting.intersect(allAppsNotDeclaringLabelButNotMissing)
  }

  def getAppsLabelViolation: Set[String] = {
    val transmitIDFANoLabel =
      getIDFATransmission.diff(getTrackingIdentifierLabelAppNames._1)
    val transmitHoneyDataNoLabel = honeyAnalysisIphone.getTrackerApps.flatMap {
      case (honeyData, apps) =>
        appsViolatingLabelHoneyData(honeyData.getLabel, apps.map(_.name).toSet)
    }
    (transmitIDFANoLabel ++ transmitHoneyDataNoLabel)
  }

  override def printNumberLabelViolation(): Unit = {

    println(s"Label Violations: ${getAppsLabelViolation.size}")
  }

  def getRequestData: (Int, Double, Double) = {
    val requests = iphoneCollection.monitoring.map(_._2.length)
    val (avg, sigma) = Statistics.statisticalAnalysis(requests)
    (requests.sum, avg, sigma)
  }

  override def printBasicNumbers(): Unit = {
    val (count, avg, sigma) = getRequestData
    println(s"Requests: $count")
    println(s"Average : $avg")
    println(s"Sigma   : $sigma")
  }

}

object CombinedDataSet {

  def readInAppsHavingPrivacyNotice(file: String): Set[String] = {
    val source = Source.fromFile(file)
    try {
      source
        .getLines()
        .flatMap { line =>
          line.split(",").toList match {
            case name :: hasDialogue :: Nil =>
              if (hasDialogue == "Dialogue") {
                List(name)
              } else {
                List()
              }
          }
        }
        .toSet
    } finally {
      source.close()
    }
  }

}
