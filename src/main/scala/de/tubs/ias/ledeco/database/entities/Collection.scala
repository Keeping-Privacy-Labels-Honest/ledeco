package de.tubs.ias.ledeco.database.entities

import wvlet.log.LogSupport

import java.time.ZonedDateTime
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}

case class Collection(id: Int,
                      phone: String,
                      start: ZonedDateTime,
                      end: ZonedDateTime,
                      monitoring: Seq[(CellphoneApplication, Seq[Request])])
    extends LogSupport {

  def merge(collections: Seq[Collection]): Collection = {
    val valid = collections.filter(_.phone != "NA")
    assert(valid.map(_.phone).toSet.size <= 1,
           s"there must be only one phone type ${}")
    val phoneType = if (valid.map(_.phone).toSet.size == 1) {
      valid.map(_.phone).toSet.head
    } else {
      "NA"
    }
    val newMonitorings = (List(monitoring) ++ valid.map(_.monitoring)).flatten
      .groupBy(_._1)
      .map { elem =>
        elem._1 -> elem._2.flatMap(_._2)
      }
      .toSeq
    Collection(
      -1,
      phoneType,
      null,
      null,
      newMonitorings
    )
  }

  def merge(collection: Collection): Collection = {
    // if the phone type is NA then the collection is empty
    if (phone == "NA") {
      collection
    } else if (collection.phone == "NA") {
      this
    } else {
      assert(
        phone == collection.phone,
        s"the collections have different phone type ${collection.phone} vs. $phone")
      Collection(
        -1,
        collection.phone,
        null,
        null,
        (monitoring ++ collection.monitoring)
          .groupBy(_._1)
          .map(elem => elem._1 -> elem._2.flatMap(_._2))
          .toSeq
      )
    }
  }

  // this is a backwards compatibility hack as we want to executions within the same collection
  // as raw data however, this cannot work with a map - consequently we use a sequence we can split()
  // into uniques. Before this monitoringMap has undefined behavior.
  val monitoringMap: Map[CellphoneApplication, Seq[Request]] = {
    val map: MMap[CellphoneApplication, ListBuffer[Request]] = MMap()
    monitoring.foreach {
      case (app, requests) =>
        if (map.contains(app)) {
          map(app).addAll(requests)
        } else {
          map.addOne(app -> ListBuffer[Request](requests: _*))
        }
    }
    map.map(elem => elem._1 -> elem._2.toSeq).toMap
  }

  def getMonitoringMap: Map[CellphoneApplication, Seq[Request]] = monitoringMap

  def split(): (Collection, Collection) = {
    val first: ListBuffer[(CellphoneApplication, Seq[Request])] = ListBuffer()
    val second: ListBuffer[(CellphoneApplication, Seq[Request])] = ListBuffer()
    monitoring.groupBy(_._1).foreach {
      case (app, values) =>
        if (values.length != 2) {
          error(
            s"when applying split on ${app.name} we only have one execution")
        } else {
          first.addOne(values.head)
          second.addOne(values.apply(1))
        }
    }
    (Collection(id, phone, start, end, first.toList),
     Collection(id, phone, start, end, second.toList))
  }

  def getRequestCountMedian: Int = {
    val sorted = monitoring.map(_._2.length).sorted
    sorted.apply((sorted.length.toFloat / 2.toFloat).floor.toInt)
  }

  def getRequestCountMean: Double = {
    val sorted = monitoring.map(_._2.length).sorted
    sorted.sum.toDouble / sorted.length.toDouble
  }

  def filter(appNames: Set[String]): Collection = {
    Collection(
      id,
      phone,
      start,
      end,
      monitoring.filter(elem => appNames.contains(elem._1.name))
    )
  }

}
