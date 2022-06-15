package de.tubs.ias.ledeco

import com.typesafe.config.{Config, ConfigFactory}
import de.halcony.argparse.{Parser, ParsingException, ParsingResult}
import de.tubs.ias.ledeco.analysis.{AnalysisMains, PrivacyLabelAnalysis}
import de.tubs.ias.ledeco.database._
import de.tubs.ias.ledeco.database.entities.CellphoneApplication
import de.tubs.ias.ledeco.screenshotter.ScreenShotEvaluator
import de.tubs.ias.ledeco.tracker.TrackerMain
import wvlet.log.LogSupport

import java.io.{File, FileWriter}
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.io.Source

object Ledeco extends LogSupport {

  val parser: Parser = Parser("ledeco", "Leak detection and comparison")
    .addSubparser(PrivacyLabelAnalysis.parser)
    .addSubparser(ScreenShotEvaluator.parser)
    .addOptional("config",
                 "c",
                 "config",
                 Some("./resources/main.conf"),
                 "the location of the config file")
    .addSubparser(
      Parser("createNameMap", "create the name map for ipa -> apk")
        .addPositional(
          "baseFolder",
          "folder below which are subfolders containing manual and matching")
        .addPositional("out", "the file to which to write the output")
        .addDefault[(ParsingResult, Config) => Unit](
          "func",
          createIpaApkNameMap,
          "the main function to be called"))
    .addSubparser(
      Parser("list")
        .addSubparser(
          Parser("app-intersection")
            .addPositional(
              "collections",
              "csv list of collections to calculate the intersection on")
            .addFlag("relaxed",
                     "r",
                     "relaxed",
                     "if set only the app name is compared")
            .addDefault[(ParsingResult, Config) => Unit](
              "func",
              showAppIntersectionMain,
              "the main function")
        )
        .addSubparser(
          Parser("collections")
            .addDefault[(ParsingResult, Config) => Unit](
              "func",
              ImportExport.listCollectionsMain,
              "the main function")))
    .addSubparser(
      Parser("import", "import the databases provided")
        .addPositional("user")
        .addPositional("password")
        .addPositional("dbname")
        .addOptional("host",
                     "l",
                     "location",
                     Some("localhost"),
                     "the host of the database")
        .addOptional("port",
                     "p",
                     "port",
                     Some("5432"),
                     "the port of the database")
        .addSubparser(Parser("remote")
          .addPositional("collections", "the collections to transfer (csv)")
          .addDefault[(ParsingResult, Config) => Unit](
            "func",
            ImportExport.importRemoteCollectionsMain))
        .addSubparser(Parser("android")
          .addDefault[(ParsingResult, Config) => Unit](
            "func",
            ImportExport.importDatabaseAndroidMain,
            "the main function"))
        .addSubparser(Parser("iphone")
          .addDefault[(ParsingResult, Config) => Unit](
            "func",
            ImportExport.importDatabaseIphoneMain,
            "the main function")))
    .addSubparser(TrackerMain.subparser)
    .addSubparser(AnalysisMains.subparser)

  def main(argv: Array[String]): Unit = {
    try {
      val pargv = parser.parse(argv)
      val conf: Config =
        ConfigFactory.parseFile(
          new File(pargv.get[Option[String]]("config").get))
      pargv.get[(ParsingResult, Config) => Unit]("func")(pargv, conf)
    } catch {
      case _: ParsingException =>
    }
  }

  private def createIpaApkNameMap(pargs: ParsingResult,
                                  @annotation.unused config: Config): Unit = {
    val baseFolder = pargs.get[String]("baseFolder")
    val map: MMap[String, MSet[String]] = MMap()
    new File(baseFolder).listFiles().filter(_.isDirectory).foreach { dir =>
      val matches = Source.fromFile(s"${dir.getAbsolutePath}/matches.csv")
      val manual = Source.fromFile(s"${dir.getAbsolutePath}/manual.csv")
      try {
        matches.getLines().map(_.split(",").toList).foreach {
          case _ :: _ :: _ :: _ :: _ :: apk :: _ :: _ :: _ :: _ :: _ :: _ :: ipa :: _ :: Nil =>
            if (map.contains(ipa)) {
              map(ipa).add(apk.replace("\"", ""))
            } else {
              map.addOne(ipa -> MSet(apk.replace("\"", "")))
            }
          case x =>
            throw new RuntimeException(
              s"unexpected split for ipa apk name map $x in matches")
        }
        manual.getLines().map(_.split(",").toList).foreach {
          case _ :: ipa :: _ :: apk :: Nil =>
            if (apk.trim() != "NA") {
              if (map.contains(ipa)) {
                map(ipa).add(apk)
              } else {
                map.addOne(ipa -> MSet(apk))
              }
            }
          case x =>
            throw new RuntimeException(
              s"unexpected split for ipa apk name map $x in manual")
        }
      } finally {
        matches.close()
        manual.close()
      }
    }
    val out = new FileWriter(new File(pargs.get[String]("out")))
    try {
      map.foreach {
        case (ipa, apkSet) => out.write(s"$ipa,${apkSet.mkString(",")}\n")
      }
    } finally {
      out.flush()
      out.close()
    }
  }

  private def getIntersectingApps(
      pargs: ParsingResult,
      config: Config): Set[(String, List[CellphoneApplication])] = {
    val connection = ImportExport.connectToMergedDatabase(config)
    val ids = pargs.get[String]("collections").split(",").map(_.toInt)
    //val appSets = getIntersectingApps(ids, mergedDataBase, pargs.get[Boolean]("relaxed"))
    val relaxed = pargs.get[Boolean]("relaxed")
    val appSets = ids.map(
      id =>
        CellphoneApplication.getUniqueElementMap(
          ImportExport.getAllApps(id, connection)))
    appSets.tail
      .foldLeft(appSets.head.map(elem => elem._1 -> List(elem._2))) {
        case (lhs, rhs) =>
          lhs
            .map {
              case (key, value) =>
                if (rhs.contains(key)) {
                  key -> (rhs(key) :: value)
                } else {
                  key -> Nil
                }
            }
            .filter(_._2.nonEmpty)
      }
      .toSet
      .filter {
        case (_, list) =>
          if (relaxed) {
            true
          } else {
            list.map(_.name).toSet.size == 1
          }
      }
  }

  def showAppIntersectionMain(pargs: ParsingResult, config: Config): Unit = {
    val appSets = getIntersectingApps(pargs, config)
    appSets.toList.sortBy(_._1).foreach { app =>
      println(app)
    }
  }

}
