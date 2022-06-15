package de.tubs.ias.ledeco.privacyLabels.elements

import de.tubs.ias.ledeco.utility.FileInteraction
import spray.json.{DefaultJsonProtocol, JsArray, JsonParser, RootJsonFormat}

case class PrivacyLabelSet(label: String,
                           absolutePath: String,
                           expectedCount: Int)

object PrivacyLabelSet extends DefaultJsonProtocol {

  implicit val privacyLabelSetFormat: RootJsonFormat[PrivacyLabelSet] =
    jsonFormat3(PrivacyLabelSet.apply)

  def readInLabelList(jsonFile: String): Seq[PrivacyLabelSet] = {
    JsonParser(FileInteraction.readFile(jsonFile))
      .asInstanceOf[JsArray]
      .elements
      .map(_.convertTo[PrivacyLabelSet])
  }

}
