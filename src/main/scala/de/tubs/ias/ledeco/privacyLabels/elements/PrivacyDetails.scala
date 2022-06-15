package de.tubs.ias.ledeco.privacyLabels.elements

import wvlet.log.Logger

case class PrivacyDetails(managePrivacyChoicesUrl: Option[String],
                          privacyTypes: List[PrivacyType]) {

  def isEqual(other: PrivacyDetails, logger: Option[Logger] = None): Boolean = {
    if (this.privacyTypes.length != other.privacyTypes.length) {
      if (logger.nonEmpty) {
        logger.get.warn("different amount of privacy types detected")
      }
      return false
    }
    (this.privacyTypes.sortBy(_.identifier) zip other.privacyTypes.sortBy(
      _.identifier)).foreach {
      case (lhs, rhs) =>
        if (!lhs.isEqual(rhs, logger)) return false
    }
    true
  }

  def printInconsistencies(logger: Logger): Unit = {
    this.privacyTypes.find(_.identifier == "DATA_NOT_COLLECTED") match {
      case Some(_) =>
        if (privacyTypes.length > 1) {
          logger.warn(s"We have both no data collection and data collection?")
        }
      case None =>
    }
    val notLinked =
      privacyTypes.find(_.identifier == "DATA_NOT_LINKED_TO_YOU") match {
        case Some(value) => value.getDataCategories.flatMap(_.dataTypes)
        case None        => List[String]()
      }
    val linked = privacyTypes.find(_.identifier == "DATA_LINKED_TO_YOU") match {
      case Some(value) => value.getDataCategories.flatMap(_.dataTypes)
      case None        => List[String]()
    }
    val intersection = notLinked.intersect(linked)
    if (intersection.nonEmpty) {
      logger.warn(
        s"we are collecting ${intersection.mkString(",")} both linked and not linked")
    }
  }

  def cleanUpLanguage(): PrivacyDetails = {
    PrivacyDetails(managePrivacyChoicesUrl, privacyTypes.map(_.cleanUpLanguage))
  }

  def getDataUsedToTrackYouCount: Int =
    privacyTypes.count(_.isInstanceOf[DataUsedToTrackYou])

  def getDataLinkedToYouCount: Int =
    privacyTypes.count(_.isInstanceOf[DataLinkedToYou])

  def getDataNotLinkedToYouCount: Int =
    privacyTypes.count(_.isInstanceOf[DataNotLinkedToYou])

  def getDataNotCollectedCount: Int =
    privacyTypes.count(_.isInstanceOf[DataNotCollected])

  def exists(privacyType: Option[String],
             purpose: Option[String],
             dataCategory: Option[String],
             dataCategoryType: Option[String]): Boolean = {
    (privacyType, purpose, dataCategory, dataCategoryType) match {
      // F F F F
      case (None, None, None, None) => privacyTypes.nonEmpty
      // T F F F
      case (Some(privacyType), None, None, None) =>
        privacyTypes.exists(_.identifier == privacyType)
      // T T F F
      case (Some(privacyType), Some(purpose), None, None) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .exists(_.getPurposes.exists(_.identifier == purpose))
      // T T F T
      case (Some(privacyType), Some(purpose), None, Some(dataType)) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .flatMap(_.getPurposes)
          .filter(_.identifier == purpose)
          .exists(_.dataCategories.exists(_.dataTypes.exists(_ == dataType)))
      // T T T F
      case (Some(privacyType), Some(purpose), Some(category), None) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .flatMap(_.getPurposes)
          .filter(_.identifier == purpose)
          .exists(_.dataCategories.exists(_.identifier == category))
      // T T T T
      case (Some(privacyType), Some(purpose), Some(category), Some(dataType)) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .flatMap(_.getPurposes)
          .filter(_.identifier == purpose)
          .flatMap(_.dataCategories)
          .filter(_.identifier == category)
          .exists(_.dataTypes.exists(_ == dataType))
      // T F T F
      case (Some(privacyType), None, Some(category), None) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .exists(_.getDataCategories.exists(_.identifier == category))
      // T F T T
      case (Some(privacyType), None, Some(category), Some(dataType)) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .flatMap(_.getDataCategories)
          .filter(_.identifier == category)
          .exists(_.dataTypes.exists(_ == dataType))
      // F T F F
      case (None, Some(purpose), None, None) =>
        privacyTypes.exists(_.getPurposes.exists(_.identifier == purpose))
      // F T T F
      case (None, Some(purpose), Some(category), None) =>
        privacyTypes
          .flatMap(_.getPurposes)
          .filter(_.identifier == purpose)
          .exists(_.dataCategories.exists(_.identifier == category))
      // F T F T
      case (None, Some(purpose), None, Some(dataType)) =>
        privacyTypes
          .flatMap(_.getPurposes)
          .filter(_.identifier == purpose)
          .exists(_.dataCategories.exists(_.dataTypes.exists(_ == dataType)))
      // F T T T
      case (None, Some(purpose), Some(category), Some(dataType)) =>
        privacyTypes
          .flatMap(_.getPurposes)
          .filter(_.identifier == purpose)
          .flatMap(_.dataCategories)
          .filter(_.identifier == category)
          .exists(_.dataTypes.exists(_ == dataType))
      // T F F T
      case (Some(privacyType), None, None, Some(dataType)) =>
        privacyTypes
          .filter(_.identifier == privacyType)
          .exists(_.getDataCategories.exists(_.dataTypes.exists(_ == dataType)))
      // F F T F
      case (None, None, Some(category), None) =>
        privacyTypes.exists(
          _.getDataCategories.exists(_.identifier == category))
      // F F T T
      case (None, None, Some(category), Some(dataType)) =>
        privacyTypes
          .flatMap(_.getDataCategories)
          .filter(_.identifier == category)
          .exists(_.dataTypes.exists(_ == dataType))
      // F F F T
      case (None, None, None, Some(dataType)) =>
        privacyTypes.exists(
          _.getDataCategories.exists(_.dataTypes.exists(_ == dataType)))
    }
  }

  def printPurposeOverview(): Unit = {
    println("Data Linked To You")
    println(
      privacyTypes
        .filter(_.isInstanceOf[DataLinkedToYou])
        .map(_.asInstanceOf[DataLinkedToYou])
        .flatMap(_.purposes.map(elem => (elem.identifier, elem.purpose)))
        .toSet
        .mkString("\n"))
    println()
    println("Data Not Linked To You")
    println(
      privacyTypes
        .filter(_.isInstanceOf[DataNotLinkedToYou])
        .map(_.asInstanceOf[DataNotLinkedToYou])
        .flatMap(_.purposes.map(elem => (elem.identifier, elem.purpose)))
        .toSet
        .mkString("\n"))
  }

  def getPrivacyTypeDataCategorySet(privacyType: String): Set[String] = {
    privacyTypes.flatMap {
      case DataLinkedToYou(_, _, _, purposes)
          if privacyType == "DATA_LINKED_TO_YOU" =>
        purposes.flatMap(_.dataCategories).flatMap(_.dataTypes)
      case DataNotLinkedToYou(_, _, _, purposes)
          if privacyType == "DATA_NOT_LINKED_TO_YOU" =>
        purposes.flatMap(_.dataCategories).flatMap(_.dataTypes)
      case DataUsedToTrackYou(_, _, _, dataCategories)
          if privacyType == "DATA_USED_TO_TRACK_YOU" =>
        dataCategories.flatMap(_.dataTypes)
      case _ => List()
    }.toSet
  }

  def getPurposeCountMap(privacyType: String): Map[String, Int] = {
    val purposes = privacyTypes.flatMap {
      case DataLinkedToYou(_, _, _, purposes)
          if privacyType == "DATA_LINKED_TO_YOU" =>
        purposes.map(_.purpose)
      case DataNotLinkedToYou(_, _, _, purposes)
          if privacyType == "DATA_NOT_LINKED_TO_YOU" =>
        purposes.map(_.purpose)
      case _ => List()
    }
    purposes.toSet.map { purpose: String =>
      purpose -> purposes.count(_ == purpose)
    }.toMap
  }

  def getPrivacyTypes: Set[String] = {
    privacyTypes.map(_.identifier).toSet
  }

  def getPurposes: Set[String] = {
    privacyTypes.flatMap(_.getPurposes).map(_.identifier).toSet
  }

  def getDataCategories: Set[String] = {
    privacyTypes.flatMap(_.getDataCategories).map(_.identifier).toSet
  }

  def getDataTypes: Set[String] = {
    privacyTypes.flatMap(_.getDataCategories).flatMap(_.dataTypes).toSet
  }

}
