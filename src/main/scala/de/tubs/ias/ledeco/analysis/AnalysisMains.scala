package de.tubs.ias.ledeco.analysis

import com.typesafe.config.Config
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.ias.ledeco.database.{ImportExport, MergedDataCollection}
import de.tubs.ias.ledeco.tracker.IdentifiedTracker
import wvlet.log.LogSupport

import java.io.File

object AnalysisMains extends LogSupport {

  val subparser: Parser =
    Parser("analysis", "analyzing a given collection (set) for information")
      .addOptional("analysisConfig",
                   "a",
                   "analysis-config",
                   Some("resources/analysisConfig.json"),
                   "the config containing the analysis meta data")
      .addSubparser(
        Parser("check", "check the config file for inconsistencies or errors")
          .addDefault[(ParsingResult, Config) => Unit]("func", checkConfigMain))
      .addSubparser(
        Parser("perform",
               "conduct an analysis action on the (sub) set of datasets")
          .addPositional("where",
                         "in what folder shall we write the result files")
          .addPositional("action", "what to do {plotting,analysis,numbers}")
          .addPositional("whom", "whom to perform the action on")
          .addSubparser(Parser("basics", "")
            .addDefault[(ParsingResult, Config) => Unit]("func", basicsMain))
          .addSubparser(Parser("apple-identifier", "")
            .addDefault[(ParsingResult, Config) => Unit]("func",
                                                         appleIdentifierMain))
          .addSubparser(Parser("honey-data", "actions related to honey-data")
            .addDefault[(ParsingResult, Config) => Unit]("func", honeyDataMain))
          .addSubparser(Parser("IDFA", "actions related to IDFA transmission")
            .addDefault[(ParsingResult, Config) => Unit]("func", IDFAMain))
          .addSubparser(Parser("hosts", "hosts receiving leaked data")
            .addDefault[(ParsingResult,Config) => Unit]("func",hostsMain))
          .addSubparser(Parser("GDPR-required",
                               "actions related to gdpr required")
            .addDefault[(ParsingResult, Config) => Unit]("func", GDPRMain))
          .addSubparser(Parser("tracker", "actions related to tracker")
            .addPositional("tracker","csv list of relevant tracker or all")
            .addDefault[(ParsingResult, Config) => Unit]("func", trackerMain)))

  private def getAnalysisConfig(pargs: ParsingResult): AnalysisConfig = {
    AnalysisConfigReader.readConfig(
      pargs.get[Option[String]]("analysisConfig").get)
  }

  def checkConfigMain(pargs: ParsingResult,
                      @annotation.unused config: Config): Unit = {
    val aconf = getAnalysisConfig(pargs)
    aconf.printSanityCheck()
  }

  private def checkWhom(config: AnalysisConfig, whom: List[String]): Unit = {
    whom.foreach { who =>
      config.dataSets.find(_.label == who) match {
        case Some(_) =>
        case None    => throw new RuntimeException(s"the dataset $who is unknown")
      }
    }
  }

  private def getWhom(config: AnalysisConfig,
                      pargs: ParsingResult): List[String] = {
    val allCollections = config.dataSets.map(_.label)
    val ret = pargs.get[String]("whom") match {
      case "all"    => allCollections
      case multiple => multiple.split(",").toList
    }
    checkWhom(config, ret)
    ret
  }

  private def createCombinedDataSet(
      pargs: ParsingResult,
      mainConfig: Config): Seq[CombinedDataSet] = {
    val config: AnalysisConfig = getAnalysisConfig(pargs)
    val whom: List[String] = getWhom(config, pargs)
    val honeyDataiPhone: Seq[HoneyData] = config.getHoneyDataFirstIphone
    val tracker: Map[String,List[IdentifiedTracker]] = config.getTrackerNames.map(name => name -> config.getTracker(name).toList).toMap
    val pattern: Seq[Pattern] = config.getPatterns
    val connection: MergedDataCollection = new MergedDataCollection(
      ImportExport.connectToMergedDatabase(mainConfig))
    val combines: List[CombinedDataSet] = whom.map { label =>
      val (goodIphone, _) =
        config.getIphoneCollection(label, connection)
      new CombinedDataSet(
        label,
        goodIphone,
        config.getDataSet(label).getIphoneIdNameMap.toSet,
        config.getDataSet(label).getPrivacyLabelAnalysis,
        tracker,
        honeyDataiPhone,
        config.getHoneyDataMapping
          .map(elem =>
            elem.honeyDataLabel -> elem.privacyLabelDataCategoryType.getOrElse(
              List()))
          .toMap,
        pattern
      )
    }
    combines
  }

  private def getCombiendDataSets(
      pargs: ParsingResult,
      mainConfig: Config): List[CombinedDataSetTrait] = {
    val combinedDataSets: List[CombinedDataSet] =
      createCombinedDataSet(pargs, mainConfig).toList
    val mergedDataSet: List[AggregatedCombinedDataSet] =
      if (combinedDataSets.length > 1) {
        List(new AggregatedCombinedDataSet(combinedDataSets))
      } else {
        List[AggregatedCombinedDataSet]()
      }
    combinedDataSets ++ mergedDataSet
  }

  private def GDPRMain(pargs: ParsingResult, mainConfig: Config): Unit = {
    val aconfig = getAnalysisConfig(pargs)
    val combinedDataSets: List[CombinedDataSetTrait] =
      getCombiendDataSets(pargs, mainConfig)
    combinedDataSets.foreach { combinedDataSet =>
      println(combinedDataSet.getLabel)
      val outFolder =
        s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
      new File(outFolder).mkdirs()
      val actions = pargs.get[String]("action") match {
        case "all" => List("plotting", "numbers")
        case x     => x.split(",").toList
      }
      actions.foreach {
        case "plotting" =>
          combinedDataSet.plotAppsThatShouldVsHavePrivacyNotice(
            outFolder,
            aconfig.getScreenshotAnalysisFile)
        case "numbers" =>
          combinedDataSet.printAppsThatShouldVsHavePrivacyNoticeNumbers(
            aconfig.getScreenshotAnalysisFile)
        case x => throw new RuntimeException(s"action $x is not recognized")
      }
    }
  }

  private def hostsMain(pargs :ParsingResult, mainConfig : Config) : Unit = {
    val combinedDataSet : List[CombinedDataSetTrait] = getCombiendDataSets(pargs, mainConfig)
    val actions = pargs.get[String]("action") match {
      case "all" => List("plotting", "numbers")
      case x     => x.split(",").toList
    }
    combinedDataSet.foreach {
      combinedDataSet =>
        val outFolder = s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
        new File(outFolder).mkdirs()
        actions.foreach {
          case "plotting" =>
            combinedDataSet.plotDrainTargetHosts(outFolder)
          case "numbers" =>
            combinedDataSet.printDrainTargetHosts()
        }
    }
  }

  private def IDFAMain(pargs: ParsingResult, mainConfig: Config): Unit = {
    val combinedDataSets: List[CombinedDataSetTrait] =
      getCombiendDataSets(pargs, mainConfig)
    combinedDataSets.foreach { combinedDataSet =>
      println(combinedDataSet.getLabel)
      val outFolder =
        s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
      new File(outFolder).mkdirs()
      val actions = pargs.get[String]("action") match {
        case "all" => List("plotting", "numbers")
        case x     => x.split(",").toList
      }
      actions.foreach {
        case "plotting" =>
          combinedDataSet.plotIDFATransmission(outFolder)
        case "numbers" =>
          combinedDataSet.printIDFANumbers()
        case x => throw new RuntimeException(s"action $x is not recognized")
      }
    }
  }

  private def appleIdentifierMain(pargs: ParsingResult,
                                  mainConfig: Config): Unit = {
    val combinedDataSets: List[CombinedDataSetTrait] =
      getCombiendDataSets(pargs, mainConfig)
    combinedDataSets.foreach { combinedDataSet =>
      val outFolder =
        s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
      val actions = pargs.get[String]("action") match {
        case "all" => List("plotting")
        case x     => List(x)
      }
      actions.foreach {
        case "plotting" =>
          new File(outFolder).mkdirs()
          combinedDataSet.plotPotentialAppleIdentifierTransmission(outFolder)
        case x => throw new RuntimeException(s"action $x is not recognized")
      }
    }
  }

  private def basicsMain(pargs: ParsingResult, mainConfig: Config): Unit = {
    val combinedDataSets: List[CombinedDataSetTrait] =
      getCombiendDataSets(pargs, mainConfig)
    combinedDataSets.foreach { combinedDataSet =>
      println()
      println(combinedDataSet.getLabel)
      val outFolder =
        s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
      new File(outFolder).mkdirs()
      val actions = pargs.get[String]("action") match {
        case "all" => List("numbers")
        case x     => List(x)
      }
      actions.foreach {
        case "numbers" =>
          combinedDataSet.printBasicNumbers()
        case x => throw new RuntimeException(s"action $x is not recognized")
      }
    }
  }

  private def honeyDataMain(pargs: ParsingResult, mainConfig: Config): Unit = {
    val combinedDataSets: List[CombinedDataSetTrait] =
      getCombiendDataSets(pargs, mainConfig)
    combinedDataSets.foreach { combinedDataSet =>
      val outFolder =
        s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
      new File(outFolder).mkdirs()
      val actions = pargs.get[String]("action") match {
        case "all" => List("analysis", "plotting", "numbers")
        case x     => List(x)
      }
      actions.foreach {
        case "analysis" =>
          combinedDataSet.analysisHoneyDataTransmission(outFolder)
        case "numbers" =>
          combinedDataSet.printHoneyDataNumbers()
          combinedDataSet.printNumberLabelViolation()
        case "plotting" =>
          new File(outFolder).mkdirs()
          combinedDataSet.plotHoneyDataTransmission(outFolder)
        case x => throw new RuntimeException(s"action $x is not recognized")
      }
    }
  }

  private def trackerMain(pargs: ParsingResult, mainConfig: Config): Unit = {
    val combinedDataSets: List[CombinedDataSetTrait] = {
      getCombiendDataSets(pargs, mainConfig)
    }
    val which : Set[String] = pargs.get[String]("tracker") match {
      case "all" => combinedDataSets.flatMap(_.getTrackerListNames).toSet
      case x     => x.split(",").toSet
    }
    val actions = pargs.get[String]("action") match {
      case "all" => List("numbers", "plotting")
      case x     => x.split(",").toList
    }
    combinedDataSets.foreach { combinedDataSet =>
      actions.foreach {
        case "plotting" =>
          val outFolder =
            s"${pargs.get[String]("where")}/${combinedDataSet.getLabel}/"
          new File(outFolder).mkdirs()
          which.foreach {
            tracker => combinedDataSet.plotTrackerAnalysis(tracker, outFolder, 20)
          }
        case "numbers" =>
          println(s"Dataset: ${combinedDataSet.getLabel}")
          which.foreach {
            tracker =>
              println()
              println(tracker)
              combinedDataSet.printTrackerNumbers(tracker)
          }
        case x => throw new RuntimeException(s"action $x is not recognized")
      }
    }
  }
}
