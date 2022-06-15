package de.tubs.ias.ledeco.tracker

import de.tubs.ias.ledeco.utility.RequestLocation.Mixed

case class ExodusTracker(name: String,
                         code_signature: String,
                         network_signature: String,
                         website: String) {

  def convertToIdentifiedTracker: List[IdentifiedTracker] = {
    network_signature
      .split("\\|")
      .toList
      .map(_.replace("\\", ""))
      .filterNot(_ == "")
      .groupBy(elem => getTld(elem))
      .map {
        case (_, domains) =>
          IdentifiedTracker(domains, Mixed, List())
      }
      .toList
  }

  private def getTld(str: String): String = {
    str.split("\\.").reverse.slice(0, 2).reverse.mkString(".")
  }

  override def toString: String = s"$name"
}
