package de.tubs.ias.ledeco.analysis

import de.halcony.StringTable

trait PrivacyLabelAnalysisTrait extends BarGraphPlottable {

  def getName: String

  def getFullDataTable(outFormat: String, filterColumn: Boolean): String

  def barPlot(outFolder: String): Unit

  def keynessPlot(outFolder: String): Unit

  val reductionMatrix: Map[String, String] = List(
    "DATA_LINKED_TO_YOU" -> "Linked",
    "DATA_NOT_LINKED_TO_YOU" -> "Not Linked",
    "DATA_NOT_COLLECTED" -> "No Collection",
    "DATA_USED_TO_TRACK_YOU" -> "Tracking",
    "ANALYTICS" -> "Analytics",
    "APP_FUNCTIONALITY" -> "Functionality",
    "DEVELOPERS_ADVERTISING" -> "Dev Ads",
    "OTHER_PURPOSES" -> "Other",
    "PRODUCT_PERSONALIZATION" -> "Product Personalization",
    "THIRD_PARTY_ADVERTISING" -> "Third Party Ads",
    "Product Personalization" -> "Personalization"
  ).toMap

  protected def rowsToTableString(rows: List[List[String]],
                                  outFormat: String): String = {
    outFormat match {
      case "pretty" =>
        val table = new StringTable(rows.head, maxWidth = 80)
        rows.tail.foreach(table.addRow(_))
        table.getTable
      case "latex" =>
        val sb = new StringBuilder()
        rows.foreach { row =>
          sb.append(
            row
              .map(elem => reductionMatrix.getOrElse(elem, elem))
              .mkString("", "&", "\\\\\n")
              .replace("%", "\\%"))
        }
        sb.toString()
    }
  }

  protected def isRelevantColumn(privacyType: Option[String],
                                 purpose: Option[String],
                                 category: Option[String],
                                 dataType: Option[String]): Boolean = {
    (privacyType, purpose, category, dataType) match {
      case (Some(_), None, None, None)    => true
      case (Some(_), Some(_), None, None) => true
      //case (Some(_),None,None,Some(_)) => true
      //case (None,Some(_),None,Some(_)) => true
      case (None, None, None, None) => false
      case _                        => false
    }
  }

  def printInconsistencies(): Unit = {}
}
