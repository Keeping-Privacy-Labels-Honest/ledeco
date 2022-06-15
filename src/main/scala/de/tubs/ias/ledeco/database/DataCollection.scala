package de.tubs.ias.ledeco.database

import de.tubs.ias.ledeco.database.entities.Collection
import de.tubs.ias.ledeco.database.entities.CellphoneApplication

trait Identifyer

trait DataCollection {

  def getCollection(id: Identifyer,
                    appRestrictions: Option[Seq[CellphoneApplication]])
    : (Collection, Seq[(CellphoneApplication, String)])

}
