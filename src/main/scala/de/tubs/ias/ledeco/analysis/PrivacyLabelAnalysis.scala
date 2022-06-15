package de.tubs.ias.ledeco.analysis

import com.typesafe.config.Config
import de.halcony.argparse.{Parser, ParsingResult}
import de.halcony.ppm.colors.{Black, Color, ColorWheel}
import de.halcony.ppm.graph.Coordinate
import de.halcony.ppm.graph.generics.Plot
import de.halcony.ppm.graph.visual.bar.BarPlot
import de.tubs.ias.ledeco.privacyLabels.elements.{LabelReader, PrivacyDetails}
import de.tubs.ias.ledeco.utility.FileInteraction
import wvlet.log.LogSupport

import collection.mutable.{Map => MMap}
import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer

class PrivacyLabelAnalysis(
    val name: String,
    val privacyLabelSet: Map[String, Option[PrivacyDetails]])
    extends LogSupport
    with PrivacyLabelAnalysisTrait {

  def getAppsWithNonEmptyPrivacyDetails: Set[String] =
    privacyLabelSet.filter {
      case (_, value) => value.nonEmpty && value.get.privacyTypes.nonEmpty
    }.keySet

  def filterExistingByRank(
      rank: List[String],
      count: Int): List[(String, Option[PrivacyDetails])] = {
    if (count == 0) {
      Nil
    } else {
      if (rank.isEmpty) {
        throw new RuntimeException(
          "the rank does not provide sufficient data to continue or there are not enough non empty")
      } else {
        privacyLabelSet(rank.head) match {
          case Some(value) =>
            if (value.privacyTypes.nonEmpty) {
              (rank.head, Some(value)) :: filterExistingByRank(rank.tail,
                                                               count - 1)
            } else {
              filterExistingByRank(rank.tail, count)
            }
          case None => filterExistingByRank(rank.tail, count)
        }
      }
    }
  }

  def filterExistingPrivacyLabelSet(privacyType: Option[String],
                                    purpose: Option[String],
                                    category: Option[String],
                                    dataType: Option[String]): Set[String] = {
    privacyLabelSet
      .filterNot(elem => elem._2.isEmpty || elem._2.get.privacyTypes.isEmpty)
      .filter {
        case (_, Some(value)) =>
          value.exists(privacyType, purpose, category, dataType)
      }
      .keySet
  }

  def merge(other: Seq[PrivacyLabelAnalysis]): PrivacyLabelAnalysis = {
    val mergedMap: MMap[String, Option[PrivacyDetails]] = MMap()
    val keys = privacyLabelSet.keys ++ other.flatMap(_.privacyLabelSet.keys)
    info(s"merging across ${keys.size} different apps")
    val allSets = List(privacyLabelSet) ++ other.map(_.privacyLabelSet)
    keys.foreach { key =>
      // None means the app is part of the set but does not exist, Some(null) means the app is not part of the set, Some(_) is pl
      val optionsUnfiltered =
        allSets.map(elem => elem.getOrElse(key, Some(null)))
      if (optionsUnfiltered.count(elem => elem.isEmpty) != 0 && optionsUnfiltered
            .exists(elem => elem.nonEmpty && elem.get != null)) {
        warn(
          s"App $key exists as empty ${optionsUnfiltered.count(elem => elem.isEmpty)} and non empty ${optionsUnfiltered
            .count(elem => elem.nonEmpty && elem.get != null)} in different datasets we will take the non empty")
      }
      val options: List[Option[PrivacyDetails]] =
        optionsUnfiltered.filter(elem => elem.nonEmpty && elem.get != null)
      if (options.isEmpty) {
        mergedMap.addOne(key -> None)
      } else if (options.length == 1) {
        mergedMap.addOne(key -> options.head)
      } else {
        //sanity check if there are different labels
        options.sliding(2).foreach {
          case lhs :: rhs :: Nil =>
            if (!lhs.get.isEqual(rhs.get, Some(this.logger))) {
              warn(s"detected mismatch in privacy label for app $key")
            }
        }
        mergedMap.addOne(key -> options.head)
      }
    }
    val mergedSet = mergedMap.toMap
    new PrivacyLabelAnalysis("merged", mergedSet)
  }

  private val nonEmpty: Seq[PrivacyDetails] = getNonEmptyPrivacyDetails

  info(s"creating number table for $name")
  private val numberTable
    : Map[String, Map[String, Map[String, Map[String, Int]]]] = getNumberTable

  override def getName: String = name

  def getNumberTableValue(privacyType: String,
                          purpose: String,
                          category: String,
                          dataType: String): Int = {
    numberTable.get(dataType) match {
      case Some(value) =>
        value.get(category) match {
          case Some(value) =>
            value.get(purpose) match {
              case Some(value) =>
                value.get(privacyType) match {
                  case Some(value) => value
                  case None        => 0
                }
              case None => 0
            }
          case None => 0
        }
      case None => 0
    }
  }

  /** Counts the non empty distributions
    *
    * @return
    */
  private def getNumberTable
    : Map[String, Map[String, Map[String, Map[String, Int]]]] = {
    val privTypes = getPrivacyTypeIdentifier.toList.sorted.map(Some(_)) ++ List(
      None)
    val purposes = List(None) ++ getPurposeIdentifier.toList.sorted.map(Some(_))
    val categories = List(None) ++ getCategoryIdentifier.toList.sorted
      .map(Some(_))
    val dataTypes = List(None) ++ getDataTypes.toList.sorted.map(Some(_))
    dataTypes.map { dataType =>
      val dataTypeName = dataType.getOrElse("any")
      dataTypeName ->
        categories.map { category =>
          val categoryName = category.getOrElse("any")
          categoryName ->
            purposes.map { purpose =>
              val purposeName = purpose.getOrElse("any")
              purposeName ->
                privTypes.map { privType =>
                  val privTypeName = privType.getOrElse("any")
                  val count = nonEmpty.count(
                    _.exists(privType, purpose, category, dataType))
                  privTypeName -> count
                }.toMap
            }.toMap
        }.toMap
    }.toMap
  }

  def getMissingCount: Int = privacyLabelSet.size - nonEmpty.length

  def getBadApps: Int = privacyLabelSet.count(_._2.isEmpty)

  def hasLabelCount: Int =
    privacyLabelSet.count(elem =>
      elem._2.nonEmpty && elem._2.get.privacyTypes.nonEmpty)

  def getExistingYetMissingLabel: Int =
    privacyLabelSet.count(elem =>
      elem._2.nonEmpty && elem._2.get.privacyTypes.isEmpty)

  def getPrivacyTypeIdentifier: Set[String] = {
    nonEmpty.flatMap(_.privacyTypes.map(_.identifier)).toSet
  }

  def getPurposeIdentifier: Set[String] = {
    nonEmpty
      .flatMap(_.privacyTypes)
      .flatMap(_.getPurposes)
      .map(_.identifier)
      .toSet
  }

  def getCategoryIdentifier: Set[String] = {
    nonEmpty
      .flatMap(_.privacyTypes)
      .flatMap(_.getDataCategories)
      .map(_.identifier)
      .toSet
  }

  def getDataTypes: Set[String] = {
    nonEmpty
      .flatMap(_.privacyTypes)
      .flatMap(_.getDataCategories)
      .flatMap(_.dataTypes)
      .toSet
  }

  def getNonEmptyPrivacyDetails: Seq[PrivacyDetails] = {
    privacyLabelSet
      .filterNot(elem => elem._2.isEmpty || elem._2.get.privacyTypes.isEmpty)
      .map(_._2.get)
      .toSeq
  }

  def getMissingPrivacyLabels: Int = {
    privacyLabelSet.count(elem =>
      elem._2.isEmpty || elem._2.get.privacyTypes.isEmpty)
  }

  def count(filter: PrivacyDetails => Boolean): Int = {
    nonEmpty.count(filter)
  }

  def getPrivacyTypeCount: Map[String, Int] = {
    getPrivacyTypeIdentifier.map { identifier =>
      identifier -> nonEmpty.count(_.exists(Some(identifier), None, None, None))
    }.toMap
  }

  private def mapMapToPlots(
      mapmap: Map[String, Map[String, Double]]): List[Plot] = {
    val colorWheel = new ColorWheel()
    mapmap.map {
      case (str, value) =>
        val color = colorWheel.getNextColor
        val coordinates = value.map {
          case (str, i) =>
            Coordinate(i.toString, str.replace("_", "-").replace(" ", "-"))
        }.toList
        new BarPlot()
          .setColor(color)
          .addData(coordinates)
          .setName(str.replace("_", "-").replace(" ", "-"))
    }.toList
  }

  def getPrivacyTypeCountPlots(fill: Color, border: Color): Seq[Plot] = {
    val fullCount: Double = privacyLabelSet.size.toDouble
    val empty: Double = getExistingYetMissingLabel.toDouble / fullCount
    val bad: Double = getBadApps.toDouble / fullCount
    val coordinates = List(
      Coordinate(empty.toString, "no-label"),
      Coordinate(bad.toString, "bad-app")
    ) ++ getPrivacyTypeCount.map {
      case (str, i) =>
        Coordinate((i.toDouble / fullCount).toString,
                   str.replace("_", "-").replace(" ", "-"))
    }
    List(
      new BarPlot()
        .setLineColor(border)
        .setFillColor(fill)
        .setName(name)
        .addData(coordinates))
  }

  def getPrivacyTypePurpose: Map[String, Map[String, Int]] = {
    getPrivacyTypeIdentifier.map { privacyIdentifier =>
      privacyIdentifier -> getPurposeIdentifier.map { purposeIdentifier =>
        purposeIdentifier -> nonEmpty.count(
          _.exists(Some(privacyIdentifier),
                   Some(purposeIdentifier),
                   None,
                   None))
      }.toMap
    }.toMap
  }

  private def convertCountMapToPercentageMap(
      map: Map[String, Map[String, Int]],
      baseCount: Int): Map[String, Map[String, Double]] = {
    map.map {
      case (key, value) =>
        key -> value.map {
          case (key, value) => key -> value.toDouble / baseCount.toDouble
        }
    }
  }

  def getPrivacyTypePurposePlots: List[Plot] = {
    mapMapToPlots(
      convertCountMapToPercentageMap(getPrivacyTypePurpose, nonEmpty.length))
  }

  def getPrivacyTypeDataType: Map[String, Map[String, Int]] = {
    getPrivacyTypeIdentifier.map { privacyType =>
      privacyType -> getDataTypes.map { dataType =>
        dataType -> nonEmpty.count(
          _.exists(Some(privacyType), None, None, Some(dataType)))
      }.toMap
    }.toMap
  }

  def getPrivacyTypeDataTypePlots: List[Plot] = {
    mapMapToPlots(
      convertCountMapToPercentageMap(getPrivacyTypeDataType, nonEmpty.length))
  }

  def getPurposeDataCategory(
      ofPrivacyType: Option[String] = None): Map[String, Map[String, Int]] = {
    getPurposeIdentifier.map { purposeIdentifier =>
      purposeIdentifier -> getCategoryIdentifier.map { categoryIdentifier =>
        categoryIdentifier -> nonEmpty.count(
          _.exists(ofPrivacyType,
                   Some(purposeIdentifier),
                   Some(categoryIdentifier),
                   None))
      }.toMap
    }.toMap
  }

  def getPurposeDataType(
      ofPrivacyType: Option[String] = None): Map[String, Map[String, Int]] = {
    getPurposeIdentifier.map { purposeIdentifier =>
      purposeIdentifier -> getDataTypes.map { dataType =>
        dataType -> nonEmpty.count(
          _.exists(ofPrivacyType,
                   Some(purposeIdentifier),
                   None,
                   Some(dataType)))
      }.toMap
    }.toMap
  }

  def getPurposeDataTypePlots(
      ofPrivacyType: Option[String] = None): List[Plot] = {
    mapMapToPlots(
      convertCountMapToPercentageMap(getPurposeDataType(ofPrivacyType),
                                     nonEmpty.length))
  }

  private def printFloat(float: Float, decimals: Int = 4): String = {
    s"%.${decimals}f".format(float)
  }

  def plotPrivacyTypeDistribution(out: String): Unit = {
    val file = s"$out/privacyTypeDistribution"
    createBarGraph(file,
                   getPrivacyTypeCountPlots(Black, Black),
                   privacyLabelSet.size)
  }

  def plotPrivacyTypePurpose(out: String): Unit = {
    val file = s"$out/privacyTypePurpose"
    createBarGraph(file, getPrivacyTypePurposePlots, privacyLabelSet.size)
  }

  def plotPrivacyTypeDataCategory(out: String): Unit = {
    val file = s"$out/privacyTypeDataCategory"
    createBarGraph(file, getPrivacyTypeDataTypePlots, privacyLabelSet.size)
  }

  def plotPurposeDataCategory(out: String): Unit = {
    var file = s"$out/purposeDataCategoryLinked"
    createBarGraph(file,
                   getPurposeDataTypePlots(Some("DATA_LINKED_TO_YOU")),
                   privacyLabelSet.size)
    file = s"$out/purposeDataCategoryNotLinked"
    createBarGraph(file,
                   getPurposeDataTypePlots(Some("DATA_NOT_LINKED_TO_YOU")),
                   privacyLabelSet.size)
  }

  override def keynessPlot(outFolder: String): Unit = {}

  override def barPlot(outFolder: String): Unit = {
    plotPrivacyTypeDistribution(outFolder)
    plotPrivacyTypePurpose(outFolder)
    plotPrivacyTypeDataCategory(outFolder)
    plotPurposeDataCategory(outFolder)
  }

  override def printInconsistencies(): Unit = {
    this.privacyLabelSet.filter(_._2.nonEmpty).foreach { elem =>
      info(s"analyzing ${elem._1}")
      elem._2.get.printInconsistencies(this.logger)
    }
  }

  override def getFullDataTable(outFormat: String,
                                filterColumns: Boolean): String = {
    val rows: ListBuffer[List[String]] = ListBuffer[List[String]]()
    rows.addOne(
      List("Apps", "Apps %", "Privacy Type", "Purpose", "Category", "Type"))
    val privTypes = getPrivacyTypeIdentifier.toList.sorted.map(Some(_)) ++ List(
      None)
    val purposes = List(None) ++ getPurposeIdentifier.toList.sorted.map(Some(_))
    val categories = List(None) ++ getCategoryIdentifier.toList.sorted
      .map(Some(_))
    val dataTypes = List(None) ++ getDataTypes.toList.sorted.map(Some(_))
    val fullSize = privacyLabelSet.size
    val missing = this.getMissingCount
    val missingPct = missing.toFloat / fullSize.toFloat
    val having = nonEmpty.length
    val havingPct = having.toFloat / fullSize.toFloat
    val bad = getBadApps
    val badPct = bad.toFloat / missing.toFloat
    val missingLabel = getExistingYetMissingLabel
    val missingLabelPct = missingLabel.toFloat / missing.toFloat
    rows.addOne(
      List(fullSize.toString, printFloat(1.00f), "all", "-", "-", "-"))
    rows.addOne(
      List(missing.toString, printFloat(missingPct), "missing", "-", "-", "-"))
    rows.addOne(
      List(bad.toString, printFloat(badPct), "bad-app", "-", "-", "-"))
    rows.addOne(
      List(missingLabel.toString,
           printFloat(missingLabelPct),
           "missing-label",
           "-",
           "-",
           "-"))
    rows.addOne(
      List(having.toString, printFloat(havingPct), "exists", "-", "-", "-"))
    dataTypes.foreach { dataType =>
      val dataTypeName = dataType.getOrElse("any")
      categories.foreach { category =>
        val categoryName = category.getOrElse("any")
        purposes.foreach { purpose =>
          val purposeName = purpose.getOrElse("any")
          privTypes.foreach { privType =>
            val privTypeName = privType.getOrElse("any")
            val count = getNumberTableValue(privTypeName,
                                            purposeName,
                                            categoryName,
                                            dataTypeName)
            if (count != 0 && (!filterColumns || isRelevantColumn(privType,
                                                                  purpose,
                                                                  category,
                                                                  dataType))) {
              val pct = count.toFloat / having
              rows.addOne(
                List(count.toString,
                     printFloat(pct),
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

object PrivacyLabelAnalysis extends LogSupport {

  def apply(name: String, folder: String): PrivacyLabelAnalysis = {
    val privacyLabelSet: Map[String, Option[PrivacyDetails]] = FileInteraction
      .jsonFiles(folder)
      .map { file =>
        val id = file.split("/").last.split("\\.").head
        val fileContent = FileInteraction.readFile(file)
        id -> (if (fileContent.contains("Resource Not Found") || fileContent
                     .contains("API capacity exceeded")) {
                 None
               } else {
                 Some(LabelReader.readPrivacyLabel(file)._2)
               })
      }
      .toMap
    new PrivacyLabelAnalysis(name, privacyLabelSet)
  }

  def apply(name: String, privacyLabelSet: Map[String, Option[PrivacyDetails]])
    : PrivacyLabelAnalysis = {
    new PrivacyLabelAnalysis(name, privacyLabelSet)
  }

  val parser: Parser =
    Parser("large-scale-privacy-label", "large scale privacy label analysis")
      .addPositional("folder",
                     "the folder containing the privacy label subfolder")
      .addPositional("where", "where to output the results")
      .addPositional(
        "what",
        "what action to conduct {analysis,bar-plotting,keyness-plotting}")
      .addOptional("drop",
                   "d",
                   "drop",
                   None,
                   "csv list of categories to ignore")
      .addFlag("equalize", "e", "equalize", "if set the lowest count is used")
      .addFlag("filterColumns",
               "f",
               "filter",
               "if set the the combinations which are counted are filtered")
      .addDefault[(ParsingResult, Config) => Unit]("func", main)

  def main(pargs: ParsingResult, @annotation.unused config: Config): Unit = {
    val folders: List[String] =
      if (new File(pargs.get[String]("folder"))
            .listFiles()
            .exists(_.isDirectory)) {
        new File(pargs.get[String]("folder"))
          .listFiles()
          .filter(_.isDirectory)
          .map(_.getAbsolutePath)
          .toList
      } else {
        List(pargs.get[String]("folder"))
      }
    val ignore = pargs.get[Option[String]]("drop") match {
      case Some(value) => value.split(",").toSet
      case None        => Set[String]()
    }
    val where = pargs.get[String]("where")
    val privacyAnalyses =
      folders.filterNot(folder => ignore.exists(folder.contains(_))).map {
        folder =>
          val name = folder.split("/").last
          PrivacyLabelAnalysis(name, folder)
      }
    val tbd =
      if (pargs.get[Boolean]("equalize") && privacyAnalyses.length > 1) {
        val equalized = new AggregatePrivacyLabelAnalysis(privacyAnalyses)
          .equalize(pargs.get[String]("folder"))
        equalized.analysis ++ List(equalized)
      } else {
        if (privacyAnalyses.length == 1) {
          privacyAnalyses
        } else {
          privacyAnalyses ++ List(
            new AggregatePrivacyLabelAnalysis(privacyAnalyses))
        }
      }
    tbd.foreach { privacyLabelAnalysis =>
      val outFolder = s"$where/${privacyLabelAnalysis.getName}/"
      new File(outFolder).mkdirs()
      info(s"running action on ${privacyLabelAnalysis.getName}")
      val actions = pargs.get[String]("what") match {
        case "all" =>
          List("analysis", "bar-plotting", "numbers", "keyness-plotting")
        case x => List(x)
      }
      actions.foreach {
        case "inconsistencies" =>
          privacyLabelAnalysis.printInconsistencies()
        case "analysis" =>
          val outFileWriterPretty =
            new FileWriter(new File(s"$outFolder/analysis.txt"))
          val outFileWriterTex =
            new FileWriter(new File(s"$outFolder/analysis.tex"))
          try {
            outFileWriterPretty.write(privacyLabelAnalysis
              .getFullDataTable("pretty", pargs.get[Boolean]("filterColumns")))
            outFileWriterTex.write(
              privacyLabelAnalysis
                .getFullDataTable("latex", pargs.get[Boolean]("filterColumns")))
          } finally {
            outFileWriterPretty.flush()
            outFileWriterPretty.close()
            outFileWriterTex.flush()
            outFileWriterTex.close()
          }
        case "bar-plotting" =>
          privacyLabelAnalysis.barPlot(outFolder)
        case "keyness-plotting" =>
          privacyLabelAnalysis match {
            case x: AggregatePrivacyLabelAnalysis => x.keynessPlot(outFolder)
            case _                                =>
          }
        case "numbers" =>
          privacyLabelAnalysis match {
            case x: AggregatePrivacyLabelAnalysis => x.printNumbers()
            case _                                =>
          }
      }
    }
  }

}
