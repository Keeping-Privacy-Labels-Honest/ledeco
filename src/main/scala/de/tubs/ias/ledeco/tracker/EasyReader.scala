package de.tubs.ias.ledeco.tracker

import de.tubs.ias.ledeco.utility.DataReader.AnalysisDataEndpointJSONReader.identifiedTrackerFormat
import de.tubs.ias.ledeco.utility.RequestLocation.Mixed
import spray.json.enrichAny
import wvlet.log.LogSupport
import collection.mutable.{Set => MSet}
import java.io.{File, FileWriter}
import scala.io.Source

object EasyReader extends LogSupport {

  def readInEasyList(filePath: String): Seq[IdentifiedTracker] = {
    val source = Source.fromFile(filePath)
    val known = MSet[String]()
    try {
      source
        .getLines()
        .filter { line =>
          line.length > 2 && line.substring(0, 2) == "||"
        }
        .map { line =>
          val split = line.substring(2).split(Array('^', '/', '#'))
          if (!(split.head.contains("*") || split.head.contains("$") || split.head == "com") && !known
                .contains(split.head)) {
            known.addOne(split.head)
            Some(
              IdentifiedTracker(List(split.head), Mixed, List())
            )
          } else {
            None
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)
        .toSeq
    } finally {
      source.close()
    }
  }

  def convertToJsonSet(inFile: String, outFolder: String): Unit = {
    val parsed = readInEasyList(inFile)
    parsed.indices.foreach { i =>
      val newFile = s"$outFolder/tracker_$i.json"
      info(s"converted: ${parsed(i).domains.mkString(",")}")
      val fw = new FileWriter(new File(newFile))
      try {
        fw.write(parsed(i).toJson.prettyPrint)
      } finally {
        fw.close()
      }
    }
  }

}
