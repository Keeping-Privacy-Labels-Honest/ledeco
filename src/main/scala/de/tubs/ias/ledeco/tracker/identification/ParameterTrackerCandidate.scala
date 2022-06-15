package de.tubs.ias.ledeco.tracker.identification

import de.tubs.ias.ledeco.utility.RequestLocation.Location

case class ParameterTrackerCandidate(location: Location,
                                     name: String,
                                     fpfr: Option[String],
                                     fpsr: Option[String],
                                     spfr: Option[String],
                                     spsr: Option[String]) {

  def hasIntraPhoneMatch: Boolean =
    fpfr == fpsr && spfr == spsr && fpfr.nonEmpty && spfr.nonEmpty

  def hasIterPhoneMismatch: Boolean = fpfr != spfr && fpsr != spsr

  def isPossibleIdentifier: Boolean = hasIntraPhoneMatch && hasIterPhoneMismatch

}

object ParameterTrackerCandidate {

  val KNOWN_TRACKER = Set(
    // https://www.analyticsmarket.com/blog/how-google-analytics-collects-data/
    ("doubleclick", "_gid"),
    ("google", "_gid")
  )

}
