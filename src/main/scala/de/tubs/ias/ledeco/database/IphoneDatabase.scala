package de.tubs.ias.ledeco.database
import de.tubs.ias.ledeco.database.MergedDataCollection.BadCollection
import de.tubs.ias.ledeco.database.entities.Collection
import de.tubs.ias.ledeco.database.entities.CellphoneApplication

class IphoneDatabase(connection: PostgresConnection) extends DataCollection {
  override def getCollection(id: Identifyer,
                             appRestrictions: Option[Seq[CellphoneApplication]])
    : (Collection, BadCollection) = {
    val (collection, _) =
      new AndroidDatabase(connection).getCollection(id, appRestrictions)
    (Collection(collection.id,
                "iphone",
                collection.start,
                collection.end,
                collection.monitoring),
     Nil)
  }
}
