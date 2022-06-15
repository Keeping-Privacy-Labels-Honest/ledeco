package de.tubs.ias.ledeco.privacyLabels.elements

import de.tubs.ias.ledeco.privacyLabels.elements.Purpose.knownPurposes
import wvlet.log.Logger

object Purpose {

  val knownPurposes: Map[String, String] = Map(
    "DEVELOPERS_ADVERTISING" -> "Developer's Advertising or Marketing",
    "THIRD_PARTY_ADVERTISING" -> "Third-Party Advertising",
    "PRODUCT_PERSONALIZATION" -> "Product Personalization",
    "OTHER_PURPOSES" -> "Other Purposes",
    "APP_FUNCTIONALITY" -> "App Functionality",
    "ANALYTICS" -> "Analytics"
  )

}

case class Purpose(identifier: String,
                   purpose: String,
                   dataCategories: Seq[DataCategory]) {

  assert(knownPurposes.contains(identifier))

  def isEqual(rhs: Purpose, logger: Option[Logger] = None): Boolean = {
    if (identifier != rhs.identifier) {
      if (logger.nonEmpty) {
        logger.get.warn(s"comparing $identifier with ${rhs.identifier}")
      }
      return false
    }
    (dataCategories.sortBy(_.identifier) zip rhs.dataCategories.sortBy(
      _.identifier)).foreach {
      case (lhs, rhs) => if (!lhs.isEqual(rhs, logger)) return false
    }
    true
  }

  def cleanUpLanguage(): Purpose = {
    Purpose(
      identifier,
      purpose,
      dataCategories.map(_.cleanUpLanguage())
    )
  }

}
