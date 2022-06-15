package de.tubs.ias.ledeco.tracker

import com.typesafe.config.Config
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.ias.ledeco.utility.DataReader
import de.tubs.ias.ledeco.utility.DataReader.AnalysisDataEndpointJSONReader.identifiedTrackerFormat
import spray.json.enrichAny
import wvlet.log.LogSupport

import java.io.{File, FileWriter}

object TrackerMain extends LogSupport {

  val subparser: Parser =
    Parser("tracker",
           "actions concerning identification or modification of tracker")
      .addSubparser(
        Parser("mergeTracker", "merge multiple folder of tracker")
          .addPositional("trackerFolder",
                         "csv list of folder of tracker to merge")
          .addPositional("out", "the folder where to output the merged folder")
          .addDefault[(ParsingResult, Config) => Unit]("func",
                                                       mergeTrackingIdsMain,
                                                       ""))
      .addSubparser(
        Parser("convertEasy",
               "convert Easy{List,Privacy} to analysis required jsons")
          .addPositional("easyTxt", "the path to the easy list")
          .addPositional("out",
                         "the folder where to store the converted tracker")
          .addDefault[(ParsingResult, Config) => Unit]("func", convertEasy, "")
      )
      .addSubparser(
        Parser("convertExodus",
               "convert exodus tracker json to identifiedTracker jsons")
          .addPositional("exodusJson",
                         "the json file containing the exodus tracker")
          .addPositional("out",
                         "the folder in which to dump the converted tracker")
          .addDefault[(ParsingResult, Config) => Unit]("func",
                                                       convertExodusMain,
                                                       ""))

  def convertEasy(pargs: ParsingResult,
                  @annotation.unused config: Config): Unit = {
    EasyReader.convertToJsonSet(pargs.get[String]("easyTxt"),
                                pargs.get[String]("out"))
  }

  def convertExodusMain(pargs: ParsingResult,
                        @annotation.unused config: Config): Unit = {
    var counter = 0
    DataReader
      .readInExodusTracker(pargs.get[String]("exodusJson"))
      .flatMap(_.convertToIdentifiedTracker)
      .filter(_.domains.nonEmpty)
      .foreach { tracker =>
        counter += 1
        val outFile = s"${pargs.get[String]("out")}/tracker_$counter.json"
        val fw = new FileWriter(new File(outFile))
        try {
          fw.write(tracker.toJson.prettyPrint)
        } finally {
          fw.close()
        }
      }
  }

  def mergeTrackingIdsMain(pargs: ParsingResult,
                           @annotation.unused config: Config): Unit = {
    var counter = 0
    val set = pargs.get[String]("trackerFolder").split(",").flatMap { folder =>
      DataReader.readInTracker(folder)
    }
    info("merging trackers")
    set
      .groupBy(_.tld)
      .map {
        case (tld, values) =>
          info(s"$tld has ${values.length} different trackers to be merged")
          IdentifiedTracker(values.flatMap(_.domains).toSet.toList,
                            values.head.location,
                            values.flatMap(_.names).toSet.toList)
      }
      .foreach { identifier: IdentifiedTracker =>
        counter += 1
        val outFile = s"${pargs.get[String]("out")}/tracker_$counter.json"
        val fw = new FileWriter(new File(outFile))
        try {
          fw.write(identifier.toJson.prettyPrint)
        } finally {
          fw.close()
        }
      }
  }
}
