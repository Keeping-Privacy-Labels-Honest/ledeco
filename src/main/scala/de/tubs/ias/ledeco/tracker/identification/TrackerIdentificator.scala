package de.tubs.ias.ledeco.tracker.identification

import de.tubs.ias.ledeco.database.entities.{CellphoneApplication, Collection}
import de.tubs.ias.ledeco.tracker.{IdentifiedTracker, identification}
import de.tubs.ias.ledeco.utility.DataReader.AnalysisDataEndpointJSONReader.identifiedTrackerFormat
import spray.json.enrichAny
import wvlet.log.LogSupport

import java.io.{File, FileWriter}
import scala.collection.mutable.{Set => MSet}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

class TrackerIdentificator(fpfr: Collection,
                           fpsr: Collection,
                           spfr: Collection,
                           spsr: Collection,
                           interactive: Boolean,
                           argtrackers: Set[IdentifiedTracker] = Set(),
                           argNoTrackers: Set[IdentifiedTracker] = Set())
    extends LogSupport {

  private var trackerMap
    : Option[Map[CellphoneApplication, Nguyen.RequestTrackers]] = None
  private val trackers: MSet[IdentifiedTracker] = {
    val buf = MSet[IdentifiedTracker]()
    argtrackers.foreach(buf.addOne)
    buf
  }
  private val noTrackers: MSet[IdentifiedTracker] = {
    val buf = MSet[IdentifiedTracker]()
    argNoTrackers.foreach(buf.addOne)
    buf
  }

  def getTrackers: Set[IdentifiedTracker] = trackers.toSet
  def getNoTrackers: Set[IdentifiedTracker] = noTrackers.toSet

  private[identification] def getTrackerCandidates
    : Map[CellphoneApplication, Nguyen.RequestTrackers] = {
    val collections = List(fpfr, fpsr, spfr, spsr)
    info("creating tracking id isolation sets for each app")
    val isolationSets: Seq[AppTrackingIdIsolationSet] =
      collections.flatMap(_.monitoringMap.keys).flatMap {
        app: CellphoneApplication =>
          collections.map(_.monitoringMap.get(app)) match {
            case Some(a) :: Some(b) :: Some(c) :: Some(d) :: Nil =>
              List(identification.AppTrackingIdIsolationSet(app, a, b, c, d))
            case x =>
              error(
                s"When trying to get matching sets we have a missing collection $x")
              List()
          }
      }
    info(
      s"analyzing each isolation set (${isolationSets.length}) for possible tracker")
    val future = Future.sequence {
      isolationSets.map { set =>
        Future {
          set.app -> Nguyen.getTracker(set)
        }
      }
    }
    Await.result(future, Inf).toMap
  }

  private def mergeTrackerAcrossApps(): Unit = {
    assert(trackerMap.nonEmpty)
    var counter = 0
    val max = trackerMap.get.size
    trackerMap.get.foreach {
      case (app, map) =>
        counter += 1
        info(s"having a hard look at the tracker of $app ($counter/$max)")
        val (retTracker, retNoTracker) =
          TrackerAggregator.aggregate(map, trackers, noTrackers, interactive)
        trackers.addAll(retTracker)
        noTrackers.addAll(retNoTracker)
    }
  }

  def generateAggregatedTracker(): Unit = {
    info("starting to analyze the datasets for possible trackers")
    trackerMap = Some(getTrackerCandidates)
    info(s"merging tracker across apps (interactive: $interactive)")
    mergeTrackerAcrossApps()
  }

  def storeTracker(folder: String): Unit = {
    info(s"sorting the tracker in folder $folder")
    var counter = 0
    trackers.foreach { tracker =>
      counter += 1
      val fileWriter =
        new FileWriter(new File(s"$folder/tracker_$counter.json"))
      try {
        fileWriter.write(tracker.toJson.prettyPrint)
      } finally {
        fileWriter.flush()
        fileWriter.close()
      }
    }
  }

}
