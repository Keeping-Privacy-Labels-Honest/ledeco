package de.tubs.ias.ledeco.privacyLabels.elements

import de.tubs.ias.ledeco.privacyLabels.elements.DataCategory.DATA_CATEGORY_TYPE_TRANSLATION
import wvlet.log.Logger

case class DataCategory(dataCategory: String,
                        dataTypes: List[String],
                        identifier: String) {

  def isEqual(rhs: DataCategory, logger: Option[Logger] = None): Boolean = {
    if (identifier != rhs.identifier) {
      if (logger.nonEmpty) {
        logger.get.warn(s"comparing $identifier with ${rhs.identifier}")
      }
      return false
    }
    if (dataTypes.length != rhs.dataTypes.length) {
      if (logger.nonEmpty) {
        logger.get.warn(s"different amount of data types")
      }
      return false
    }
    if (dataTypes.toSet != rhs.dataTypes.toSet) {
      if (logger.nonEmpty) {
        logger.get.warn(s"different amount of data types")
      }
      false
    } else {
      true
    }
  }

  def cleanUpLanguage(): DataCategory = {
    DataCategory(
      dataCategory,
      dataTypes
        .map { elem =>
          DataCategory.CLEAN_UP_MAPPING.find {
            case (regexp, _) =>
              val hit = regexp == elem
              hit
          } match {
            case Some(value) =>
              value._2
            case None => elem
          }
        }
        .map(
          elem =>
            DATA_CATEGORY_TYPE_TRANSLATION.getOrElse(
              elem,
              throw new RuntimeException(
                s"i do not know how to translate $elem"))),
      identifier
    )
  }

}

object DataCategory {

  val DATA_CATEGORY_TYPE_TRANSLATION: Map[String, String] =
    List[(String, String)](
      "Ungefährer Standort" -> "Coarse Location",
      "Werbedaten" -> "Advertising Data",
      "Browser-Verlauf" -> "Browsing History",
      "Kundendienst" -> "Customer Support",
      "Einkaufsverlauf" -> "Purchase History",
      "Telefonnummer" -> "Phone Number",
      "Physische Adresse" -> "Physical Address",
      "Produkt\u00ADinteraktion" -> "Product Interaction",
      "E-Mails oder Textnachrichten" -> "Emails or Text Messages",
      "Sonstige Finanz\u00ADinformationen" -> "Other Financial Info",
      "Kontakte" -> "Contacts",
      "Vertrauliche Daten" -> "Sensitive Information",
      "Sonstige Benutzer-Kontaktinforma\u00ADtionen" -> "Other Contact Information",
      "Gesundheit" -> "Health",
      "Fotos oder Videos" -> "Photos or Videos",
      "Suchverlauf" -> "Search History",
      "Geräte-ID" -> "Device-ID",
      "Benutzer-ID" -> "User-ID",
      "Fitness" -> "Fitness",
      "Genauer Standort" -> "Precise Location",
      "Sonstige Datentypen" -> "Other Data Types",
      "Leistungsdaten" -> "Performance Data",
      "Sonstige Nutzungsdaten" -> "Other Usage Data",
      "E-Mail-Adresse" -> "Email Address",
      "Bonitäts\u00ADinformationen" -> "Credit Info",
      "Spielszenen\u00ADinhalte" -> "Gameplay Content",
      "Sonstige Benutzerinhalte" -> "Other User Content",
      "Zahlungsdaten" -> "Payment Info",
      "Sonstige Diagnosedaten" -> "Other Diagnostic Data",
      "Audio-Daten" -> "Audio Data",
      "Crash-Daten" -> "Crash Data",
      "Name" -> "Name"
    ).toMap

  // we are in the 21th century and still cannot manage encoding - the fuck
  protected val CLEAN_UP_MAPPING: Set[(String, String)] = Set(
    //"Sonstige.+tionen" -> "Andere Financial Information",
    "Bonit?ts?informationen" -> "Bonitäts\u00ADinformationen",
    "Ger?te-ID" -> "Geräte-ID",
    "Sonstige Benutzer-Kontaktinforma?tionen" -> "Sonstige Benutzer-Kontaktinforma\u00ADtionen",
    "Produkt?interaktion" -> "Produkt\u00ADinteraktion",
    "Sonstige Finanz?informationen" -> "Sonstige Finanz\u00ADinformationen",
    "Spielszenen?inhalte" -> "Spielszenen\u00ADinhalte",
    "Ungef?hrer Standort" -> "Ungefährer Standort",
  )
}
