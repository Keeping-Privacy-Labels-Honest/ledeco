package de.tubs.ias.ledeco.privacyLabels.elements

import wvlet.log.{LogSupport, Logger}

trait PrivacyType {
  val identifier: String
  val description: String
  val privacyType: String

  def cleanUpLanguage: PrivacyType

  def getDataCategories: List[DataCategory]

  def getPurposes: List[Purpose]

  def isEqual(rhs: PrivacyType, logger: Option[Logger] = None): Boolean
}

case class DataUsedToTrackYou(override val identifier: String,
                              override val description: String,
                              override val privacyType: String,
                              dataCategory: List[DataCategory])
    extends PrivacyType
    with LogSupport {

  assert(identifier == "DATA_USED_TO_TRACK_YOU")

  def getDataCategories: List[DataCategory] = dataCategory

  override def getPurposes: List[Purpose] = List()

  override def isEqual(rhs: PrivacyType,
                       logger: Option[Logger] = None): Boolean = {
    assert(rhs.isInstanceOf[DataUsedToTrackYou])
    if (dataCategory.length != rhs
          .asInstanceOf[DataUsedToTrackYou]
          .dataCategory
          .length) {
      if (logger.nonEmpty) {
        logger.get.warn(
          "different amount of data categories detected for tracking")
        return false
      }
    }
    (dataCategory.sortBy(_.identifier) zip rhs.getDataCategories.sortBy(
      _.identifier)).foreach {
      case (lhs, rhs) =>
        if (!lhs.isEqual(rhs, logger)) return false
    }
    true
  }

  override def cleanUpLanguage: DataUsedToTrackYou = {
    DataUsedToTrackYou(
      identifier,
      description,
      privacyType,
      dataCategory.map(_.cleanUpLanguage())
    )
  }

}

trait DataSomehowRelatedToYou extends PrivacyType with LogSupport {
  val purposes: List[Purpose]

  def getDataCategories: List[DataCategory] = purposes.flatMap(_.dataCategories)

  override def getPurposes: List[Purpose] = purposes

  def getIdentifier: String

  override def isEqual(rhs: PrivacyType, logger: Option[Logger]): Boolean = {
    assert(rhs.isInstanceOf[DataSomehowRelatedToYou])
    if (this.getIdentifier != rhs
          .asInstanceOf[DataSomehowRelatedToYou]
          .getIdentifier) {
      if (logger.nonEmpty) {
        logger.get.warn(s"comparing unrelated somehow related data")
      }
      return false
    }
    if (purposes.length != rhs.getPurposes.length) {
      if (logger.nonEmpty) {
        logger.get.warn(
          s"different amount of purposes in ${this.getClass.toString}")
      }
      return false
    }
    (purposes.sortBy(_.identifier) zip rhs.getPurposes.sortBy(_.identifier))
      .foreach {
        case (lhs, rhs) => if (!lhs.isEqual(rhs, logger)) return false
      }
    true
  }

}

case class DataLinkedToYou(override val identifier: String,
                           override val description: String,
                           override val privacyType: String,
                           override val purposes: List[Purpose])
    extends DataSomehowRelatedToYou {
  assert(identifier == "DATA_LINKED_TO_YOU")

  override def getIdentifier: String = identifier

  override def cleanUpLanguage: DataLinkedToYou = {
    DataLinkedToYou(
      identifier,
      description,
      privacyType,
      purposes.map(_.cleanUpLanguage())
    )
  }

}

case class DataNotLinkedToYou(override val identifier: String,
                              override val description: String,
                              override val privacyType: String,
                              override val purposes: List[Purpose])
    extends DataSomehowRelatedToYou {

  assert(identifier == "DATA_NOT_LINKED_TO_YOU")

  override def getIdentifier: String = identifier

  override def cleanUpLanguage: DataNotLinkedToYou = {
    DataNotLinkedToYou(
      identifier,
      description,
      privacyType,
      purposes.map(_.cleanUpLanguage())
    )
  }

}

case class DataNotCollected(override val identifier: String,
                            override val description: String,
                            override val privacyType: String)
    extends PrivacyType {

  override def isEqual(rhs: PrivacyType, logger: Option[Logger]): Boolean = {
    assert(rhs.isInstanceOf[DataNotCollected])
    true
  }

  def getDataCategories: List[DataCategory] = List()

  override def getPurposes: List[Purpose] = List()

  override def cleanUpLanguage: PrivacyType = this

}
