package de.tubs.ias.ledeco.database.entities

import collection.mutable.{Map => MMap}

object CellphoneApplication {

  def getUniqueElementMap(
      apps: Set[CellphoneApplication]): Map[String, CellphoneApplication] = {
    val knownElements = MMap[String, Int]()
    apps.foreach { app =>
      app.name.split("\\.").foreach { elem =>
        if (knownElements.contains(elem)) {
          knownElements(elem) += 1
        } else {
          knownElements.addOne(elem -> 1)
        }
      }
    }
    apps.map { app =>
      val mostSignificant = app.name.split("\\.").last
      if (knownElements(mostSignificant) > 1) {
        app.name -> app
      } else {
        mostSignificant -> app
      }
    }.toMap
  }

}

case class CellphoneApplication(name: String, version: String)

case class Cookie(name: String, values: String)

case class Header(name: String, values: String)
