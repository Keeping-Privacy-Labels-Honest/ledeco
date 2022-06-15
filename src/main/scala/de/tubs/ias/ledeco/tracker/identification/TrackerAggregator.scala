package de.tubs.ias.ledeco.tracker.identification

import de.halcony.StringTable
import de.tubs.ias.ledeco.tracker.IdentifiedTracker
import de.tubs.ias.ledeco.tracker.identification.Nguyen.RequestTrackers
import de.tubs.ias.ledeco.utility.RequestLocation.Mixed
import de.tubs.ias.ledeco.utility.{UserInteraction, UtilityCollection}

import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}

object TrackerAggregator {

  /** aggregate the potential tracker into verified identified tracker
    *
    * @param trackerMap  the map of url to trackers
    * @param interactive whether to confirm each aggregated tracker by the user
    * @return a sequence of identified tracker
    */
  def aggregate(trackerMap: RequestTrackers,
                knownTracker: MSet[IdentifiedTracker],
                knownNotTracker: MSet[IdentifiedTracker],
                interactive: Boolean = false)
    : (Seq[IdentifiedTracker], Seq[IdentifiedTracker]) = {
    val noTracker: ListBuffer[IdentifiedTracker] = ListBuffer()
    val tracker: ListBuffer[IdentifiedTracker] = ListBuffer()
    // map all tracker by domain
    sortByDomain(trackerMap).foreach {
      // for each domain specific tracker
      case (domain, trackers) =>
        // sort the tracker by names
        sortByName(trackers).foreach {
          // for each name of the tracker
          trackers =>
            val trackerCandidate =
              IdentifiedTracker(List(domain), Mixed, List(trackers._1))
            // if interactive is deactivated or manual verification confirmed
            if (!knownNotTracker.contains(trackerCandidate) && !knownTracker.contains(
                  trackerCandidate) && (!interactive || manualVerification(
                  domain,
                  trackers._1,
                  trackers._2))) {
              // merge the tracker for the different paths
              tracker.addOne(trackerCandidate)
            } else {
              // only happens if manually disregarded ; will be ignored then
              noTracker.addOne(trackerCandidate)
            }
        }
    }
    (tracker.toList, noTracker.toList)
  }

  /** interact with the user to verify that a tracker is actually a tracker
    *
    * @param domain   the domain we are currently looking at
    * @param name     the name of the tracker
    * @param trackers the tuple of trackers sharing that name with their corresponding paths
    * @return whether the user agrees that this is a tracker
    */
  private def manualVerification(
      domain: String,
      name: String,
      trackers: Seq[(ParameterTrackerCandidate, String)]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    sb.append(s"Domain: $domain\n")
    sb.append(s"Name  : $name\n\n")
    val table = new StringTable(
      List("path", "location", "fpfr", "fpsr", "spfr", "spsr"),
      maxWidth = 50)
    trackers.sortBy(_._2).foreach {
      case (tracker, path) =>
        table.addRow(
          List(
            path,
            tracker.location.toString,
            tracker.fpfr.getOrElse("N/A"),
            tracker.fpsr.getOrElse("N/A"),
            tracker.spfr.getOrElse("N/A"),
            tracker.spsr.getOrElse("N/A")
          ))
    }
    sb.append(table.getTable)
    println(sb.toString())
    UserInteraction.yesNo("Is this a valid tracker")
  }

  /** maps the given tracker candidates by name
    *
    * @param trackers the trackers to be sorted
    * @return the mapping of tracker name to the tuples of tracker and path
    */
  private def sortByName(trackers: Seq[(ParameterTrackerCandidate, String)])
    : Map[String, Seq[(ParameterTrackerCandidate, String)]] = {
    val nameMap: MMap[String, ListBuffer[(ParameterTrackerCandidate, String)]] =
      MMap()
    trackers.foreach { tracker =>
      if (nameMap.contains(tracker._1.name)) {
        nameMap(tracker._1.name).addOne(tracker)
      } else {
        nameMap.addOne(tracker._1.name -> ListBuffer(tracker))
      }
    }
    nameMap.map { case (key, value) => key -> value.toSeq }.toMap
  }

  /** map the given request tracker by domain to tuple(tracker, path)
    *
    * @param trackerMap the map of request url to sequence of tracker
    * @return the described mapping
    */
  private def sortByDomain(trackerMap: RequestTrackers)
    : Map[String, Seq[(ParameterTrackerCandidate, String)]] = {
    val domainMap
      : MMap[String, ListBuffer[(ParameterTrackerCandidate, String)]] = MMap()
    trackerMap.foreach {
      case (url, tracker) =>
        val (domain, path) = UtilityCollection.splitUrl(url)
        // if we already know the domain
        if (domainMap.contains(domain)) {
          // just add the tracker, path tuple
          domainMap(domain).addAll(tracker.map(elem => (elem, path)))
        } else {
          // else add the domain to the map as well
          domainMap.addOne(
            domain -> ListBuffer(tracker.map(elem => (elem, path)): _*))
        }
    }
    domainMap.map { case (key, value) => key -> value.toSeq }.toMap
  }

}
