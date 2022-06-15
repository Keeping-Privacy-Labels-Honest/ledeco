package de.tubs.ias.ledeco.tracker.identification

import de.tubs.ias.ledeco.database.entities.{Parameters, Request}
import de.tubs.ias.ledeco.utility.RequestLocation.{
  Body,
  Cookie,
  Location,
  URL,
  Header
}
import wvlet.log.LogSupport

object Nguyen extends LogSupport {

  val SKIP_HEADER = Set("content-length")
  type RequestTrackers = Map[String, Seq[ParameterTrackerCandidate]]

  def getTracker(isolationSet: AppTrackingIdIsolationSet): RequestTrackers = {
    groupBySharedRequest(isolationSet)
      .map {
        case (key, (a, b, c, d)) =>
          key -> getAllRequestParameterTrackerCandidates(a, b, c, d)
      }
      .map {
        case (key, candidates) =>
          key -> candidates.filter(_.isPossibleIdentifier)
      }
  }

  /** returns a list of tracker id candidates based on shared location and name in the request across the runs
    *
    * @param fpfr sequence of requests first phone first run
    * @param fpsr sequence of requests first phone second run
    * @param spfr sequence of requests second phone first run
    * @param spsr sequence of requests second phone second run
    * @return a sequence of candidates of tracking ids
    */
  private[identification] def getAllRequestParameterTrackerCandidates(
      fpfr: Seq[Request],
      fpsr: Seq[Request],
      spfr: Seq[Request],
      spsr: Seq[Request]): Seq[ParameterTrackerCandidate] = {
    // this helper function merges all requests to the same url concerning parameters
    def mergeParameterSet(set: Seq[Request]): Parameters =
      //current configuration aggregateMerge is true meaning different parameter values are concatenated
      set.map(_.getParameters).reduce((lhs, rhs) => Parameters.merge(lhs, rhs))

    val fpfrParameter = mergeParameterSet(fpfr)
    val fpsrParameter = mergeParameterSet(fpsr)
    val spfrParameter = mergeParameterSet(spfr)
    val spsrParameter = mergeParameterSet(spsr)
    getAllRequestParameterTrackerCandidatesUtil(URL,
                                                fpfrParameter,
                                                fpsrParameter,
                                                spfrParameter,
                                                spsrParameter) ++
      getAllRequestParameterTrackerCandidatesUtil(Cookie,
                                                  fpfrParameter,
                                                  fpsrParameter,
                                                  spfrParameter,
                                                  spsrParameter) ++
      getAllRequestParameterTrackerCandidatesUtil(Body,
                                                  fpfrParameter,
                                                  fpsrParameter,
                                                  spfrParameter,
                                                  spsrParameter)
    getAllRequestParameterTrackerCandidatesUtil(Header,
                                                fpfrParameter,
                                                fpsrParameter,
                                                spfrParameter,
                                                spsrParameter)
  }

  /** utility function to access the different request locations
    *
    * @param location the location within the request (URL, Cookie, Body)
    * @param fpfr     sequence of requests first phone first run
    * @param fpsr     sequence of requests first phone second run
    * @param spfr     sequence of requests second phone first run
    * @param spsr     sequence of requests second phone second run
    * @return a sequence of candidates of tracking ids within the specified location
    */
  private[identification] def getAllRequestParameterTrackerCandidatesUtil(
      location: Location,
      fpfr: Parameters,
      fpsr: Parameters,
      spfr: Parameters,
      spsr: Parameters): Seq[ParameterTrackerCandidate] = {
    val identifier: Set[String] = location match {
      case URL    => List(fpfr, fpsr, spfr, spsr).flatMap(_.url.keys).toSet
      case Cookie => List(fpfr, fpsr, spfr, spsr).flatMap(_.cookie.keys).toSet
      case Body   => List(fpfr, fpsr, spfr, spsr).flatMap(_.body.keys).toSet
      case Header => List(fpfr, fpsr, spfr, spsr).flatMap(_.header.keys).toSet
    }
    identifier
      .map { ident =>
        location match {
          case URL =>
            Some(
              ParameterTrackerCandidate(URL,
                                        ident,
                                        fpfr.url.get(ident),
                                        fpsr.url.get(ident),
                                        spfr.url.get(ident),
                                        spsr.url.get(ident)))
          case Cookie =>
            Some(
              ParameterTrackerCandidate(Cookie,
                                        ident,
                                        fpfr.cookie.get(ident),
                                        fpsr.cookie.get(ident),
                                        spfr.cookie.get(ident),
                                        spsr.cookie.get(ident)))
          case Body =>
            Some(
              ParameterTrackerCandidate(Body,
                                        ident,
                                        fpfr.body.get(ident),
                                        fpsr.body.get(ident),
                                        spfr.body.get(ident),
                                        spsr.body.get(ident)))
          case Header =>
            if (SKIP_HEADER.contains(ident.toLowerCase)) {
              None
            } else {
              Some(
                ParameterTrackerCandidate(Header,
                                          ident,
                                          fpfr.header.get(ident),
                                          fpsr.header.get(ident),
                                          spfr.header.get(ident),
                                          spsr.header.get(ident)))
            }
        }
      }
      .filter(_.nonEmpty)
      .map(_.get)
      .toSeq
  }

  /** Groups the requests to the app into requests to the same url (domain/path)
    *
    * @param isolationSet the isolation set containing the collected requests to the app
    * @return a mapping from request url to corresponding requests (if a request url does not occur in every run the element is dropped)
    */
  private[identification] def groupBySharedRequest(
      isolationSet: AppTrackingIdIsolationSet)
    : Map[String, (Seq[Request], Seq[Request], Seq[Request], Seq[Request])] = {
    val fpfr: Map[String, Seq[Request]] =
      isolationSet.firstPhoneFirstRun.groupBy(_.getRequestUrl(true))
    val fpsr: Map[String, Seq[Request]] =
      isolationSet.firstPhoneSecondRun.groupBy(_.getRequestUrl(true))
    val spfr: Map[String, Seq[Request]] =
      isolationSet.secondPhoneFirstRun.groupBy(_.getRequestUrl(true))
    val spsr: Map[String, Seq[Request]] =
      isolationSet.secondPhoneSecondRun.groupBy(_.getRequestUrl(true))
    List(fpfr, fpsr, spfr, spsr)
      .flatMap(_.keys)
      .toSet
      .map { key: String =>
        key -> (fpfr.getOrElse(key, List()), fpsr.getOrElse(key, List()), spfr
          .getOrElse(key, List()), spsr.getOrElse(key, List()))
      }
      .toMap
      .filter { pair =>
        val (_, (first, second, third, fourth)) = pair
        if (first.nonEmpty && second.nonEmpty && third.nonEmpty && fourth.nonEmpty) {
          true
        } else {
          false
        }
      }
  }

}
