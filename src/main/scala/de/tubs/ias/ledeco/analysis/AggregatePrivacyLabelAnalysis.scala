package de.tubs.ias.ledeco.analysis

import de.halcony.ppm.colors.ColorWheel
import de.tubs.ias.ledeco.privacyLabels.elements.PrivacyLabelElements.{
  CATEGORY,
  DATA_TYPE,
  PRIVACY_TYPE,
  PURPOSE,
  PrivacyLabelElement
}
import de.tubs.ias.ledeco.utility.Statistics.statisticalAnalysis
import de.tubs.ias.ledeco.utility.{
  DualColorWheel,
  FileInteraction,
  KeynessPlot,
  Statistics
}
import de.tubs.ipa.ThreeUDownloader.ThreeUResponse
import de.tubs.ipa.ThreeUDownloader.ThreeUResponseReader.ThreeUResponseFormat
import spray.json.{JsArray, JsonParser}
import wvlet.log.LogSupport

import java.io.File
import scala.collection.mutable.ListBuffer

class AggregatePrivacyLabelAnalysis(val analysis: Seq[PrivacyLabelAnalysis])
    extends PrivacyLabelAnalysisTrait
    with LogSupport {

  private def getRankings(files: List[String]): Map[String, List[String]] = {
    files.map { file =>
      val name =
        file.split("/").last.split("\\.").reverse.tail.reverse.mkString(".")
      val response = JsonParser(FileInteraction.readFile(file))
        .asInstanceOf[JsArray]
        .elements
        .map(_.convertTo[ThreeUResponse])
      name -> response.flatMap(_.list.map(_.itemid)).toList
    }.toMap
  }

  def equalize(folder: String): AggregatePrivacyLabelAnalysis = {
    val count = analysis.map(_.hasLabelCount).min
    info(s"equalizing the sets to $count apps")
    val files = new File(folder)
      .listFiles()
      .filter(_.getAbsolutePath.endsWith(s".json"))
      .map(_.getAbsolutePath)
      .toList
    assert(files.length == analysis.length,
           "we seem to have a mismatch of rankings")
    val rankings = getRankings(files)
    val newAna = rankings.map {
      case (key, ranking) =>
        analysis.find(_.name == key) match {
          case Some(value) =>
            new PrivacyLabelAnalysis(
              value.name,
              value.filterExistingByRank(ranking, count).toMap)
          case None =>
            throw new RuntimeException(s"the set for $key is missing")
        }
    }
    assert(
      newAna.map(_.privacyLabelSet.size).toSet.size == 1,
      s"the size of the privacy label analysis are not equal ${newAna.map(_.privacyLabelSet.size)}")
    new AggregatePrivacyLabelAnalysis(newAna.toList)
  }

  val merge: PrivacyLabelAnalysis = analysis.head.merge(analysis.tail)

  override def getName: String = "merged"

  def getNames: Seq[String] = analysis.map(_.name)

  def getPurposeIdentifier: Set[String] =
    analysis.flatMap(_.getPurposeIdentifier).toSet

  def getCategoryIdentifier: Set[String] =
    analysis.flatMap(_.getCategoryIdentifier).toSet

  def getPrivacyTypeIdentifier: Set[String] =
    analysis.flatMap(_.getPrivacyTypeIdentifier).toSet

  def getDataTypes: Set[String] = analysis.flatMap(_.getDataTypes).toSet

  private def printDouble(double: Double, decimals: Int = 4): String = {
    s"%.${decimals}f".format(double)
  }

  def plotPrivacyTypeDistributions(out: String): Unit = {

    val colorWheel = new DualColorWheel()
    val plots = analysis.flatMap { elem =>
      val (fill, border) = colorWheel.getNextColors
      elem.getPrivacyTypeCountPlots(fill, border)
    }
    val outFile =
      s"$out/${analysis.map(_.getName).mkString("_")}privacyTypeDistribution"
    val max = analysis.map(_.privacyLabelSet.size).max
    createBarGraph(outFile, plots, max)

    analysis.grouped(8).foreach { elems =>
      val colorWheel = new ColorWheel()
      val plots = elems.flatMap { elem =>
        val color = colorWheel.getNextColor
        elem.getPrivacyTypeCountPlots(color, color)
      }
      val outFile =
        s"$out/${elems.map(_.getName).mkString("_")}privacyTypeDistribution"
      val max = analysis.map(_.privacyLabelSet.size).max
      createBarGraph(outFile, plots, max)
    }
  }

  private def printNumbersPrivacyTypeDC(privacyType: String): Unit = {
    val dataTypes = getDataTypes.map { dt =>
      (dt,
       merge.getNonEmptyPrivacyDetails.count(
         _.exists(Some(privacyType), None, None, Some(dt))))
    }
    //println(dataTypes)
    val (mostDt, mostCount) = dataTypes.maxBy(_._2)
    if (mostCount > 0) {
      val (mostAvg, mostSigma) = statisticalAnalysis(
        analysis.map(_.getNonEmptyPrivacyDetails.count(
          _.exists(Some(privacyType), None, None, Some(mostDt)))))
      val (fewestDt, fewestCount) = dataTypes.minBy(_._2)
      val (fewestAvg, fewestSigma) = statisticalAnalysis(
        analysis.map(_.getNonEmptyPrivacyDetails.count(
          _.exists(Some(privacyType), None, None, Some(fewestDt)))))
      println()
      println(s"Most DT         : $mostDt")
      println(s"Most DT Count   : $mostCount")
      println(s"Most Avg        : $mostAvg")
      println(s"Most Sigma      : $mostSigma")
      println()
      println(s"Fewest DT       : $fewestDt")
      println(s"Fewest DT Count : $fewestCount")
      println(s"Fewest Avg      : $fewestAvg")
      println(s"Fewest Sigma    : $fewestSigma")
    }
  }

  private def printNumbersPrivacyTypePurpose(privacyType: String): Unit = {
    val dataTypes = getPurposeIdentifier.map { purpose =>
      (purpose,
       merge.getNonEmptyPrivacyDetails.count(
         _.exists(Some(privacyType), Some(purpose), None, None)))
    }
    val (mostDt, mostCount) = dataTypes.maxBy(_._2)
    if (mostCount > 0) {
      val (mostAvg, mostSigma) = statisticalAnalysis(
        analysis.map(_.getNonEmptyPrivacyDetails.count(
          _.exists(Some(privacyType), Some(mostDt), None, None))))
      val (fewestDt, fewestCount) = dataTypes.minBy(_._2)
      val (fewestAvg, fewestSigma) = statisticalAnalysis(
        analysis.map(_.getNonEmptyPrivacyDetails.count(
          _.exists(Some(privacyType), Some(fewestDt), None, None))))
      println()
      println(s"Most P         : $mostDt")
      println(s"Most P Count   : $mostCount")
      println(s"Most Avg        : $mostAvg")
      println(s"Most Sigma      : $mostSigma")
      println()
      println(s"Fewest P       : $fewestDt")
      println(s"Fewest P Count : $fewestCount")
      println(s"Fewest Avg      : $fewestAvg")
      println(s"Fewest Sigma    : $fewestSigma")
    }
  }

  private def calculateZScore(fewestCount: Int,
                              outlierName: String,
                              overallCollect: Set[String]): Double = {
    val outlierElems =
      this.analysis.find(_.name == outlierName).get.privacyLabelSet.keySet
    val restPopulationElements = merge.privacyLabelSet.keySet.diff(outlierElems)
    val restPopulationCount = overallCollect.diff(outlierElems)
    Statistics.zTest(fewestCount,
                     outlierElems.size,
                     restPopulationCount.size,
                     restPopulationElements.size)
  }

  private def printNumbersPrivacyType(privacyType: String): Unit = {
    val overallCount = merge.getNonEmptyPrivacyDetails.count(
      _.exists(Some(privacyType), None, None, None))
    val analysisData: Seq[(String, Int)] = analysis.map { ana =>
      (ana.name,
       ana.getNonEmptyPrivacyDetails.count(
         _.exists(Some(privacyType), None, None, None)))
    }
    val collectsPrivacyType: Set[String] =
      merge.filterExistingPrivacyLabelSet(Some(privacyType), None, None, None)
    // calculation for most
    val (mostCategory, mostCount) = analysisData.maxBy(_._2)
    val mostZScore =
      calculateZScore(mostCount, mostCategory, collectsPrivacyType)
    // calculation for fewest
    val (fewstCategory, fewestCount) = analysisData.minBy(_._2)
    val fewestZScore =
      calculateZScore(fewestCount, fewstCategory, collectsPrivacyType)
    val numbers = analysisData.map(_._2)
    val (avg, sigma) = statisticalAnalysis(numbers)
    println(s"$privacyType")
    println(s"count : $overallCount")
    println(s"Most  : $mostCategory  / $mostCount / $mostZScore")
    println(s"Fewest: $fewstCategory / $fewestCount / $fewestZScore")
    println(s"Avg   : $avg")
    println(s"Sigma : $sigma")
    printNumbersPrivacyTypePurpose(privacyType)
    println()
    printNumbersPrivacyTypeDC(privacyType)
  }

  def printNumberMissing(description: String,
                         overall: Int,
                         analysises: Seq[(String, Int)]): Unit = {
    val (most, mostCount) = analysises.maxBy(_._2)
    val (min, minCount) = analysises.minBy(_._2)
    val (avg, sigma) = statisticalAnalysis(analysises.map(_._2))
    println(s"$description")
    println(s"Overall      : $overall")
    println(s"Most         : $most")
    println(s"Most Count   : $mostCount")
    println(s"Fewest       : $min")
    println(s"Fewest Count : $minCount")
    println(s"Avg          : $avg")
    println(s"Sigma        : $sigma")
  }

  def printNumbers(): Unit = {
    val missingLabelOrAppCount = merge.getExistingYetMissingLabel + merge.getBadApps
    val missingLabelOrAppAnalysisData =
      analysis.map(elem =>
        (elem.name, elem.getExistingYetMissingLabel + elem.getBadApps))
    printNumberMissing("Missing Label or App",
                       missingLabelOrAppCount,
                       missingLabelOrAppAnalysisData)
    println()

    val noAppCount = merge.getBadApps
    val noAppAnalysisData = analysis.map(elem => (elem.name, elem.getBadApps))
    printNumberMissing("No App", noAppCount, noAppAnalysisData)
    println()

    val missingLabelCount = merge.getExistingYetMissingLabel
    val noLabelAnalysisData =
      analysis.map(elem => (elem.name, elem.getExistingYetMissingLabel))
    printNumberMissing("No Label", missingLabelCount, noLabelAnalysisData)
    println()

    merge.getPrivacyTypeIdentifier.foreach { pt =>
      printNumbersPrivacyType(pt)
      println()
      println()
    }
    println()
    val appsCollectingDeviceIDNotLinked = merge.getNonEmptyPrivacyDetails.count(
      _.exists(Some("DATA_NOT_LINKED_TO_YOU"), None, None, Some("Device-ID")))
    val appsCollectingUserIDNotLinked = merge.getNonEmptyPrivacyDetails.count(
      _.exists(Some("DATA_NOT_LINKED_TO_YOU"), None, None, Some("User-ID")))
    val appsCollectingPhoneNumberNotLinked =
      merge.getNonEmptyPrivacyDetails.count(
        _.exists(Some("DATA_NOT_LINKED_TO_YOU"),
                 None,
                 None,
                 Some("Phone Number")))
    val appsCollectingEmailNotLinked = merge.getNonEmptyPrivacyDetails.count(
      _.exists(Some("DATA_NOT_LINKED_TO_YOU"),
               None,
               None,
               Some("Email Address")))
    println(
      s"Apps Collecting Device Id Not Linked: $appsCollectingDeviceIDNotLinked")
    println(
      s"Apps Collecting User Id Not Linked: $appsCollectingUserIDNotLinked")
    println(
      s"Apps Collecting Phone Number Not Linked: $appsCollectingPhoneNumberNotLinked")
    println(s"Apps Collecting Email Not Linked: $appsCollectingEmailNotLinked")
  }

  def plotKeynessPrivacyTypeDataType(outFolder: String,
                                     privacyType: String): Unit = {
    val privacyTypeSize =
      merge.getNumberTableValue(privacyType, "any", "any", "any")
    // this line compares against ALL other apps
    val remainingPrivacyTypes = getPrivacyTypeIdentifier -- Set(privacyType)
    // this line compares against all other COLLECTING apps
    val remainingPrivacyTypesSize = remainingPrivacyTypes.map { pt =>
      merge.getNumberTableValue(pt, "any", "any", "any")
    }.sum
    val privacyTypeMap: Map[String, Int] = getDataTypes.map { dataType =>
      dataType -> merge.getNumberTableValue(privacyType, "any", "any", dataType)
    }.toMap
    val remainingPrivacyTypesMap: Map[String, Int] = getDataTypes.map {
      dataType =>
        dataType -> remainingPrivacyTypes.map { pt =>
          merge.getNumberTableValue(pt, "any", "any", dataType)
        }.sum
    }.toMap
    new KeynessPlot(privacyType,
                    privacyTypeSize,
                    privacyTypeMap,
                    "rest",
                    remainingPrivacyTypesSize,
                    remainingPrivacyTypesMap).generateKeynessGraph
      .compile(s"$outFolder/keynessPrivacyType${privacyType}DataTypes.tex")
  }

  def plotKeynessPurposeDataType(outFolder: String, purpose: String): Unit = {
    val purposeSize = merge.getNumberTableValue("any", purpose, "any", "any")
    val remainingPurposes = getPurposeIdentifier -- Set(purpose)
    val remainingPurposesSize = remainingPurposes.map { p =>
      merge.getNumberTableValue("any", p, "any", "any")
    }.sum
    val purposeMap: Map[String, Int] = getDataTypes.map { dt =>
      dt -> merge.getNumberTableValue("any", purpose, "any", dt)
    }.toMap
    val remainingPurposeMap: Map[String, Int] = getDataTypes.map { dt =>
      dt -> remainingPurposes.map { p =>
        merge.getNumberTableValue("any", p, "any", dt)
      }.sum
    }.toMap
    new KeynessPlot(purpose,
                    purposeSize,
                    purposeMap,
                    "rest",
                    remainingPurposesSize,
                    remainingPurposeMap).generateKeynessGraph
      .compile(s"$outFolder/keynessPurpose${purpose}DataTypes.tex")
  }

  def getAggregatedIndividualAppsWithNonEmptyPrivacyDetailsCount(
      labels: Set[String]): Int = {
    this.analysis
      .filter(analysis => labels.contains(analysis.name))
      .flatMap(_.getAppsWithNonEmptyPrivacyDetails)
      .toSet
      .size
  }

  def countInIndividualSets(labels: Set[String],
                            specs: (Option[String],
                                    Option[String],
                                    Option[String],
                                    Option[String])): Int = {
    this.analysis
      .filter(analysis => labels.contains(analysis.name))
      .flatMap { analysis =>
        val (privacyType, purpose, dataCategory, dataType) = specs
        analysis.filterExistingPrivacyLabelSet(privacyType,
                                               purpose,
                                               dataCategory,
                                               dataType)
      }
      .toSet
      .size
  }

  def plotKeynesForOf(label: String,
                      outFolder: String,
                      of: PrivacyLabelElement): Unit = {
    // the set of apps we want to compare
    val inLabel = Set(label)
    val inLabelSize =
      getAggregatedIndividualAppsWithNonEmptyPrivacyDetailsCount(inLabel)
    // with the remaining apps
    val outLabel = this.analysis.map(_.name).toSet -- inLabel
    val outLabelSize =
      getAggregatedIndividualAppsWithNonEmptyPrivacyDetailsCount(outLabel)

    // preparing the category matching tuples
    val (privacyTypes, purposes, categories, dataTypes) = of match {
      case PRIVACY_TYPE =>
        val ident: Set[Option[String]] = getPrivacyTypeIdentifier.map(Some(_))
        val anyList = (1 to ident.size).map(_ => None)
        (ident, anyList, anyList, anyList)
      case PURPOSE =>
        val ident: Set[Option[String]] = getPurposeIdentifier.map(Some(_))
        val anyList = (1 to ident.size).map(_ => None)
        (anyList, ident, anyList, anyList)
      case CATEGORY =>
        val ident: Set[Option[String]] = getCategoryIdentifier.map(Some(_))
        val anyList = (1 to ident.size).map(_ => None)
        (anyList, anyList, ident, anyList)
      case DATA_TYPE =>
        val ident: Set[Option[String]] = getDataTypes.map(Some(_))
        val anyList = (1 to ident.size).map(_ => None)
        (anyList, anyList, anyList, ident)
    }
    val filterTuples =
      (privacyTypes lazyZip purposes lazyZip categories lazyZip dataTypes).toList
    // generating the in label count map
    val inLabelMap: Map[String, Int] = filterTuples.map { accessTuple =>
      val nonAny: String = List(accessTuple._1,
                                accessTuple._2,
                                accessTuple._3,
                                accessTuple._4).filter(_.nonEmpty).head.get
      nonAny -> countInIndividualSets(inLabel, accessTuple)
    }.toMap
    // generating the out label count map
    val outLabelMap: Map[String, Int] = filterTuples.map { accessTuple =>
      val nonAny: String = List(accessTuple._1,
                                accessTuple._2,
                                accessTuple._3,
                                accessTuple._4).filter(_.nonEmpty).head.get
      nonAny -> countInIndividualSets(outLabel, accessTuple)
    }.toMap

    new KeynessPlot(label,
                    inLabelSize,
                    inLabelMap,
                    "rest",
                    outLabelSize,
                    outLabelMap).generateKeynessGraph
      .compile(s"$outFolder/keynessAppType$label$of.tex")
  }

  def plotKeynessLinkedNotLinkedPurposes(outFolder: String): Unit = {
    val DATALINKED = Some("DATA_LINKED_TO_YOU")
    val DATAUNLINKED = Some("DATA_NOT_LINKED_TO_YOU")
    val linkedCount =
      merge.filterExistingPrivacyLabelSet(DATALINKED, None, None, None).size
    val unlinkedCount =
      merge.filterExistingPrivacyLabelSet(DATAUNLINKED, None, None, None).size
    val linkedMap: Map[String, Int] = getPurposeIdentifier.map { purpose =>
      purpose -> merge
        .filterExistingPrivacyLabelSet(DATALINKED, Some(purpose), None, None)
        .size
    }.toMap
    val unlinkedMap: Map[String, Int] = getPurposeIdentifier.map { purpose =>
      purpose -> merge
        .filterExistingPrivacyLabelSet(DATAUNLINKED, Some(purpose), None, None)
        .size
    }.toMap
    new KeynessPlot("linked purpose",
                    linkedCount,
                    linkedMap,
                    "unlinked purpose",
                    unlinkedCount,
                    unlinkedMap).generateKeynessGraph
      .compile(s"$outFolder/keynessLinkedUnlinkedPurposes.tex")
  }

  override def keynessPlot(outFolder: String): Unit = {
    analysis.filter(_ => true).map(_.name).foreach { label =>
      info("generating keyness plots for privacy types")
      plotKeynesForOf(label: String, outFolder: String, PRIVACY_TYPE)
      info("generating keyness plots for privacy purposes")
      plotKeynesForOf(label: String, outFolder: String, PURPOSE)
      info("generating keyness plots for privacy categories")
      plotKeynesForOf(label: String, outFolder: String, CATEGORY)
      info("generating keyness plots for privacy data types")
      plotKeynesForOf(label: String, outFolder: String, DATA_TYPE)
    }
    info("generating keyness for privacy types against data types")
    getPrivacyTypeIdentifier.foreach(pt =>
      plotKeynessPrivacyTypeDataType(outFolder, pt))
    info("generating keyness for purposes against data types")
    getPurposeIdentifier.foreach(pt =>
      plotKeynessPurposeDataType(outFolder, pt))
    info("generating keyness for linked/notlinked purposes")
    plotKeynessLinkedNotLinkedPurposes(outFolder)
  }

  override def barPlot(outFolder: String): Unit = {
    plotPrivacyTypeDistributions(outFolder)
    merge.plotPrivacyTypeDistribution(outFolder)
    merge.plotPrivacyTypePurpose(outFolder)
    merge.plotPrivacyTypeDataCategory(outFolder)
    merge.plotPurposeDataCategory(outFolder)
  }

  override def getFullDataTable(outFormat: String,
                                filterColumns: Boolean): String = {
    val rows: ListBuffer[List[String]] = ListBuffer[List[String]]()
    rows.addOne(
      List("Apps",
           "Apps %",
           "Avg",
           "Sigma",
           "Privacy Type",
           "Purpose",
           "Category",
           "Type"))
    val privTypes = getPrivacyTypeIdentifier.toList.sorted.map(Some(_)) ++ List(
      None)
    val purposes = List(None) ++ getPurposeIdentifier.toList.sorted.map(Some(_))
    val categories = List(None) ++ getCategoryIdentifier.toList.sorted
      .map(Some(_))
    val dataTypes = List(None) ++ getDataTypes.toList.sorted.map(Some(_))
    val fullSize = merge.privacyLabelSet.size
    val (sizeAvg, sizeSigma) = statisticalAnalysis(
      analysis.map(_.privacyLabelSet.size))
    val missing = merge.getMissingCount
    val missingPct = missing.toFloat / fullSize.toFloat
    val (missingAvg, missingSigma) = statisticalAnalysis(
      analysis.map(_.getMissingCount))
    val having = merge.getNonEmptyPrivacyDetails.size
    val havingPct = having.toFloat / fullSize.toFloat
    val (havingAvg, havingSigma) = statisticalAnalysis(
      analysis.map(_.getNonEmptyPrivacyDetails.size))
    val bad = merge.getBadApps
    val badPct = bad.toFloat / fullSize.toFloat
    val (badAvg, badSigma) = statisticalAnalysis(analysis.map(_.getBadApps))
    val missingLabel = merge.getExistingYetMissingLabel
    val missingLabelPct = missingLabel.toFloat / fullSize.toFloat
    val (missingLabelAvg, missingLabelSigma) = statisticalAnalysis(
      analysis.map(_.getExistingYetMissingLabel))
    rows.addOne(
      List(fullSize.toString,
           printDouble(1.00f),
           printDouble(sizeAvg),
           printDouble(sizeSigma),
           "all",
           "-",
           "-",
           "-"))
    rows.addOne(
      List(missing.toString,
           printDouble(missingPct),
           printDouble(missingAvg),
           printDouble(missingSigma),
           "missing",
           "-",
           "-",
           "-"))
    rows.addOne(
      List(bad.toString,
           printDouble(badPct),
           printDouble(badAvg),
           printDouble(badSigma),
           "bad app",
           "-",
           "-",
           "-"))
    rows.addOne(
      List(missingLabel.toString,
           printDouble(missingLabelPct),
           printDouble(missingLabelAvg),
           printDouble(missingLabelSigma),
           "missing label",
           "-",
           "-",
           "-"))
    rows.addOne(
      List(having.toString,
           printDouble(havingPct),
           printDouble(havingAvg),
           printDouble(havingSigma),
           "exists",
           "-",
           "-",
           "-"))
    dataTypes.foreach { dataType =>
      val dataTypeName = dataType.getOrElse("any")
      categories.foreach { category =>
        val categoryName = category.getOrElse("any")
        purposes.foreach { purpose =>
          val purposeName = purpose.getOrElse("any")
          privTypes.foreach { privType =>
            val privTypeName = privType.getOrElse("any")
            val count = merge.getNumberTableValue(privTypeName,
                                                  purposeName,
                                                  categoryName,
                                                  dataTypeName)
            val numbers = analysis.map(
              _.getNumberTableValue(privTypeName,
                                    purposeName,
                                    categoryName,
                                    dataTypeName))
            val (avg, sigma) = statisticalAnalysis(numbers)
            if (count != 0 && (!filterColumns || isRelevantColumn(privType,
                                                                  purpose,
                                                                  category,
                                                                  dataType))) {
              val pct = count.toFloat / having
              rows.addOne(
                List(count.toString,
                     printDouble(pct),
                     printDouble(avg),
                     printDouble(sigma),
                     privTypeName,
                     purposeName,
                     categoryName,
                     dataTypeName)
              )
            }
          }
        }
      }
    }
    rowsToTableString(rows.toList, outFormat)
  }

}
