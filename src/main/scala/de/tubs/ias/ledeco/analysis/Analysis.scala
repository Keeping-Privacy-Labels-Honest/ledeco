package de.tubs.ias.ledeco.analysis

import de.halcony.StringTable
import de.tubs.ias.ledeco.database.entities.{CellphoneApplication, Collection}
import wvlet.log.LogSupport

import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}

class Analysis(collection: Collection, analysisData: Seq[AnalysisData])
    extends LogSupport {

  protected val appToTrackers
    : MMap[CellphoneApplication, ListBuffer[AnalysisResultMatch]] = MMap()
  protected val trackerToApps
    : MMap[AnalysisData, ListBuffer[CellphoneApplication]] = MMap()

  def getHosts(ignore: Set[String]): Map[AnalysisData, Map[String, Int]] = {
    val ret = MMap[AnalysisData, MMap[String, Int]]()
    appToTrackers.values.foreach { arml =>
      val already = MSet[String]()
      arml.foreach { arm =>
        // only count host once for each App
        val hostSplit = arm.request.host.split("\\.")
        val tldHost =
          hostSplit.slice(hostSplit.length - 2, hostSplit.length).mkString(".")
        if (!already.contains(tldHost)) {
          already.addOne(tldHost)
          if (!ignore.contains(arm.analysisData.getLabel)) {
            if (!ret.contains(arm.analysisData)) {
              ret.addOne(arm.analysisData -> MMap())
            }
            if (ret(arm.analysisData).contains(tldHost)) {
              ret(arm.analysisData)(tldHost) += 1
            } else {
              ret(arm.analysisData).addOne(tldHost -> 1)
            }
          }
        }
      }
    }
    ret.map(elem => elem._1 -> elem._2.toMap).toMap
  }

  def getCollection: Collection = collection

  def getAnalysisData: Seq[AnalysisData] = analysisData

  def getAppToMatches: Map[CellphoneApplication, List[AnalysisResultMatch]] =
    appToTrackers.map { case (key, value) => key -> value.toList }.toMap

  def getAppToTrackers: Map[CellphoneApplication, List[AnalysisData]] =
    appToTrackers.map {
      case (key, value) => key -> value.toList.map(_.analysisData)
    }.toMap

  def getTrackerApps: Map[AnalysisData, List[CellphoneApplication]] =
    trackerToApps.map { case (key, value) => key -> value.toList }.toMap

  def filterAgainstAppNames(names: Set[String]): Analysis = {
    val filteredCollection =
      Collection(
        this.getCollection.id,
        this.getCollection.phone,
        this.getCollection.start,
        this.getCollection.end,
        this.getCollection.monitoring.filter(elem =>
          names.contains(elem._1.name))
      )
    new Analysis(filteredCollection, this.getAnalysisData)
  }

  def run(): Analysis = {
    analysisData.foreach { tracker =>
      //info(s"checking for occurrence to tracker in tld $tracker")
      collection.monitoringMap.foreach {
        case (app, requests) =>
          val arm = tracker.isContained(requests)
          if (arm.nonEmpty) {
            if (appToTrackers.contains(app)) {
              appToTrackers(app).addAll(arm)
            } else {
              appToTrackers.addOne(
                app -> ListBuffer[AnalysisResultMatch](arm: _*))
            }
            if (trackerToApps.contains(tracker)) {
              trackerToApps(tracker).addOne(app)
            } else {
              trackerToApps.addOne(tracker -> ListBuffer(app))
            }
          } else {
            if (!trackerToApps.contains(tracker)) {
              trackerToApps.addOne(tracker -> ListBuffer())
            }
            if (!appToTrackers.contains(app)) {
              appToTrackers.addOne(app -> ListBuffer())
            }
          }
      }
    }
    this
  }

  def output(mode: String): String = {
    assert(Set("CSV", "TABLE").contains(mode),
           "the only allowed modes are CSV or TABLE")
    val appToTrackerSeq = appToTrackers.map {
      case (app, trackers) => (app.name, trackers.length)
    }
    val trackerToAppsSeq = trackerToApps.map {
      case (tracker, apps) =>
        (tracker.getLabel, apps.length)
    }
    val sb: StringBuilder = new StringBuilder()
    mode match {
      case "CSV" =>
        sb.append("Apps and their tracker\n")
        appToTrackerSeq.foreach {
          case (app, trackerCount) => sb.append(s"$app,$trackerCount\n")
        }
        sb.append("Tracker and their apps\n")
        trackerToAppsSeq.foreach {
          case (label, appCount) =>
            sb.append(s"$label,$appCount\n")
        }
      case "TABLE" =>
        val appToTrackerTable =
          new StringTable(List("App", "TrackerCount"), maxWidth = 40)
        appToTrackerSeq.foreach {
          case (app, trackerCount) =>
            appToTrackerTable.addRow(List(app, trackerCount.toString))
        }
        val trackerToAppTable =
          new StringTable(List("label", "AppCount"), maxWidth = 40)
        trackerToAppsSeq.foreach {
          case (label, appcount) =>
            trackerToAppTable.addRow(List(label, appcount.toString))
        }
        sb.append("Apps and their tracker\n")
        sb.append(appToTrackerTable.getTable)
        sb.append("Tracker and their apps\n")
        sb.append(trackerToAppTable.getTable)
    }
    sb.toString()
  }

}
