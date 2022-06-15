package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.database.MergedDataCollection.BadCollection
import de.tubs.ias.ledeco.database.entities.Collection
import de.tubs.ias.ledeco.database.{
  Identifyer,
  MergedDataCollection,
  NumericCollectionIdentifyer
}
import de.tubs.ias.ledeco.tracker.IdentifiedTracker
import de.tubs.ias.ledeco.utility.{DataReader, FileInteraction}
import spray.json.{
  DefaultJsonProtocol,
  JsArray,
  JsString,
  JsValue,
  JsonParser,
  RootJsonFormat
}
import wvlet.log.LogSupport

import java.io.File
import scala.io.Source

case class AnalysisDataSet(label: String,
                           appCount: Int,
                           firstUserCollectionsIphone: List[Int],
                           secondUserCollectionsIphone: List[Int],
                           firstUserCollectionsAndroid: List[Int],
                           secondUserCollectionsAndroid: List[Int],
                           iphoneIdNameMap: String,
                           privacyLabelFolder: String)
    extends LogSupport {

  def getPrivacyLabelAnalysis: PrivacyLabelAnalysis = {
    PrivacyLabelAnalysis(label, privacyLabelFolder)
  }

  def getIphoneIdNameMap: List[(String, String)] = {
    val source = Source.fromFile(iphoneIdNameMap)
    try {
      source
        .getLines()
        .map { line =>
          val split = line.split(",")
          (split(2), split(1))
        }
        .toList
    } finally {
      source.close()
    }
  }

  private def checkCollection(collection: List[Int], name: String): Boolean = {
    info(s"Checking collection $name....")
    if (collection.nonEmpty) {
      info(s"SUC: collection contains ${collection.length} entries")
      true
    } else {
      error("ERR: collection does not contain any entries")
      false
    }
  }

  private def checkPrivacyLabelConsistency(): Boolean = {
    var success = true
    if (new File(privacyLabelFolder).exists()) {
      info("SUC: file exists")
      try {
        val entries = PrivacyLabelAnalysis(label, privacyLabelFolder)
        if (entries.privacyLabelSet.nonEmpty) {
          info(
            s"SUC: file $privacyLabelFolder contains ${entries.privacyLabelSet.size} entries")
          this.getIphoneIdNameMap.foreach {
            case (id, name) =>
              if (!entries.privacyLabelSet.contains(id)) {
                error(s"ERR: We do not have a label for ipa ($name/$id)")
                success = false
              }
          }
        } else {
          error(s"ERR: file does not contain any entries")
          success &&= false
        }
      } catch {
        case x: Throwable =>
          error(s"ERR: ${x.getMessage}")
          success &&= false
      }
    }
    success
  }

  def conductSanityCheck(): Boolean = {
    var success = true
    info(s"Checking dataset $label...")
    success &&= checkCollection(firstUserCollectionsIphone, "fuci")
    success &&= checkCollection(secondUserCollectionsIphone, "suci")
    success &&= checkCollection(firstUserCollectionsAndroid, "fuca")
    success &&= checkCollection(secondUserCollectionsAndroid, "suca")
    info("Checking iphone id map...")
    if (new File(iphoneIdNameMap).exists()) {
      info("SUC: file exists")
      if (getIphoneIdNameMap.nonEmpty) {
        info(s"SUC: file contains ${getIphoneIdNameMap.length} entries")
      } else {
        error(s"ERR: file does not contain any entries")
        success &&= false
      }
    } else {
      error("ERR: file does not exist")
      success &&= false
    }
    info("Checking privacy labels...")
    success = checkPrivacyLabelConsistency() && success
    success
  }

}

case class HoneyDataMapping(honeyDataLabel: String,
                            privacyLabelDataCategoryType: Option[List[String]])

case class AnalysisConfig(trackerFolder: Map[String, String],
                          honeyDataFirstIphoneJson: String,
                          honeyDataSecondIphoneJson: String,
                          patternJson: String,
                          chatterJsonIphone: String,
                          chatterJsonAndroid: String,
                          screenshotAnalysis: String,
                          iphoneAndroidAppNameTranslation: String,
                          honeyDataMapping: List[HoneyDataMapping],
                          dataSets: List[AnalysisDataSet])
    extends LogSupport {

  def getAllDataSetLabels: Seq[String] = dataSets.map(_.label)

  def getHoneyDataMapping: List[HoneyDataMapping] = honeyDataMapping

  def getScreenshotAnalysisFile: String = screenshotAnalysis

  private val iphoneAndroidAppNameMap: Map[String, String] = {
    try {
      val source = Source.fromFile(iphoneAndroidAppNameTranslation)
      try {
        source
          .getLines()
          .map { line =>
            line.split(",").toList match {
              case iphone :: android :: remaining =>
                if (remaining.nonEmpty) {
                  warn(s"we have a non unique matching $iphone -> $android + (${remaining
                    .mkString(",")})")
                }
                iphone -> android
            }
          }
          .toMap
      } finally {
        source.close()
      }
    } catch {
      case _: Throwable => Map[String, String]()
    }
  }
  private val androidIphoneAppNameMap: Map[String, String] =
    iphoneAndroidAppNameMap.map(elem => elem._2 -> elem._1)

  private val ipaIdNameMap: Map[String, String] = {
    dataSets.flatMap { dataset =>
      try {
        dataset.getIphoneIdNameMap
      } catch {
        case _: Throwable => Map()
      }
    }.toMap
  }

  private val ipaNameIdMap: Map[String, String] =
    ipaIdNameMap.map(elem => elem._2 -> elem._1)

  def getIpaName(id: String): Option[String] = ipaIdNameMap.get(id)
  def getIpaId(name: String): Option[String] = ipaNameIdMap.get(name)

  def getAndroidPendant(ipa: String): Option[String] =
    iphoneAndroidAppNameMap.get(ipa)
  def getIphonePendant(apk: String): Option[String] =
    androidIphoneAppNameMap.get(apk)

  def getHoneyDataFirstIphone: Seq[HoneyData] =
    DataReader.readInHoneyData(honeyDataFirstIphoneJson)
  def getHoneyDataSecondIphone: Seq[HoneyData] =
    DataReader.readInHoneyData(honeyDataSecondIphoneJson)
  def getTracker(name: String): Seq[IdentifiedTracker] =
    DataReader.readInTracker(
      trackerFolder.getOrElse(
        name,
        throw new RuntimeException(s"the tracker $name is unknown")))
  def getTrackerNames: Set[String] = trackerFolder.keySet
  def getPatterns: Seq[Pattern] = DataReader.readInPatterns(patternJson)

  def getIphoneChatter: Seq[Chatter] =
    DataReader.readInChatter(chatterJsonIphone)
  def getAndroidChatter: Seq[Chatter] = {
    if (new File(chatterJsonAndroid).exists()) {
      DataReader.readInChatter(chatterJsonAndroid)
    } else {
      warn("android chatter json does not exist")
      Seq()
    }
  }

  private def getNoChatterCollection(
      connection: MergedDataCollection,
      identifier: Identifyer,
      chatter: Seq[Chatter]): (Collection, BadCollection) = {
    val (good, bad) = connection.getCollection(identifier, None)
    (Chatter.purgeChatter(good, chatter), bad)
  }

  def getDataSet(label: String): AnalysisDataSet = {
    dataSets
      .find(_.label == label)
      .getOrElse(
        throw new RuntimeException(s"there is no collection name $label"))
  }

  def getIphoneCollection(
      label: String,
      connection: MergedDataCollection,
      noChatter: Boolean = true,
      first: Boolean = true): (Collection, BadCollection) = {
    val dataSet = dataSets
      .find(_.label == label)
      .getOrElse(
        throw new RuntimeException(s"the collection $label is unknown"))
    val collectionIds =
      if (first) dataSet.firstUserCollectionsIphone
      else dataSet.secondUserCollectionsIphone
    getNoChatterCollection(connection,
                           NumericCollectionIdentifyer(collectionIds.head),
                           if (!noChatter) Seq() else getIphoneChatter)
  }

  def getAndroidCollection(
      label: String,
      connection: MergedDataCollection,
      noChatter: Boolean = true,
      first: Boolean = true): (Collection, BadCollection) = {
    val dataSet = dataSets
      .find(_.label == label)
      .getOrElse(
        throw new RuntimeException(s"the collection $label is unknown"))
    val collectionIds =
      if (first) dataSet.firstUserCollectionsAndroid
      else dataSet.secondUserCollectionsAndroid
    if (collectionIds.length == 1) {
      val (good, bad) = getNoChatterCollection(
        connection,
        NumericCollectionIdentifyer(collectionIds.head),
        if (noChatter) Seq() else getAndroidChatter)
      (good.split()._1, bad)
    } else {
      warn("There is no android collection - returning empty/nulled collection")
      (Collection(-1, "NA", null, null, Seq()), Seq())
    }
  }

  def printSanityCheck(): Unit = {
    assert(new File("/home/").exists(),
           "just making sure exists works as intended")
    var success = true
    // check if honey data exists
    success = sanityCheckAnalysisDataFile(DataReader.readInHoneyData,
                                          honeyDataFirstIphoneJson,
                                          "honey data first iphone") && success
    success = sanityCheckAnalysisDataFile(DataReader.readInHoneyData,
                                          honeyDataSecondIphoneJson,
                                          "honey data second iphone") && success
    // check exodus tracker
    trackerFolder.foreach {
      case (name, folder) =>
        success = sanityCheckAnalysisDataFile(DataReader.readInTracker,
                                              folder,
                                              s"$name tracker") && success
    }
    // check pattern
    success = sanityCheckAnalysisDataFile(DataReader.readInPatterns,
                                          patternJson,
                                          "patterns") && success
    // check chatter iphone
    success = sanityCheckAnalysisDataFile(DataReader.readInChatter,
                                          chatterJsonIphone,
                                          "iphone chatter") && success
    // check chatter android
    success = sanityCheckAnalysisDataFile(DataReader.readInChatter,
                                          chatterJsonAndroid,
                                          "android chatter") && success
    // check iphone android app name translation
    info("Checking iphone/android name translation file...")
    if (new File(iphoneAndroidAppNameTranslation).exists()) {
      info("SUC: file exists")
      if (androidIphoneAppNameMap.nonEmpty) {
        info(s"SUC: file contains ${androidIphoneAppNameMap.size} entries")
      } else {
        error(s"ERR: file does not contain any entries")
        success = false
      }
    } else {
      error("ERR: file does not exist")
      success = false
    }
    // checking all datasets
    dataSets.foreach { dataSet =>
      success = dataSet.conductSanityCheck() && success
    }
    if (success) {
      info("SUCCESS")
    } else {
      error("FAILURE")
    }
  }

  private def sanityCheckAnalysisDataFile(
      fileParser: String => Seq[AnalysisData],
      file: String,
      name: String): Boolean = {
    info(s"Checking $name...")
    if (new File(file).exists()) {
      info("SUC: file exists")
      try {
        val analysisData = fileParser(file)
        if (analysisData.nonEmpty) {
          info(s"SUC: file contains ${analysisData.length} entries")
          true
        } else {
          error(s"ERR: file does not contain any entries")
          false
        }
      } catch {
        case x: Throwable =>
          println(s"ERR: ${x.getMessage}")
          false
      }
    } else {
      error("ERR: file does not exist")
      false
    }
  }

}

object AnalysisConfigReader extends DefaultJsonProtocol {

  implicit val honeyDataMappingFormat: RootJsonFormat[HoneyDataMapping] =
    jsonFormat2(HoneyDataMapping)

  implicit object AnalysisDataSetFormat
      extends RootJsonFormat[AnalysisDataSet] {
    override def write(obj: AnalysisDataSet): JsValue =
      throw new NotImplementedError()

    override def read(json: JsValue): AnalysisDataSet = {
      val fields = json.asJsObject.fields
      AnalysisDataSet(
        fields("label").asInstanceOf[JsString].value,
        fields("appCount").convertTo[Int],
        fields("firstUserCollectionsIphone")
          .asInstanceOf[JsArray]
          .elements
          .map(_.convertTo[Int])
          .toList,
        fields("secondUserCollectionsIphone")
          .asInstanceOf[JsArray]
          .elements
          .map(_.convertTo[Int])
          .toList,
        fields("firstUserCollectionsAndroid")
          .asInstanceOf[JsArray]
          .elements
          .map(_.convertTo[Int])
          .toList,
        fields("firstUserCollectionsAndroid")
          .asInstanceOf[JsArray]
          .elements
          .map(_.convertTo[Int])
          .toList,
        fields("iphoneIdNameMap").asInstanceOf[JsString].value,
        fields("privacyLabelFolder").asInstanceOf[JsString].value
      )
    }
  }

  implicit object AnalysisConfigFormat extends RootJsonFormat[AnalysisConfig] {
    override def write(obj: AnalysisConfig): JsValue =
      throw new NotImplementedError()

    override def read(json: JsValue): AnalysisConfig = {
      val fields = json.asJsObject.fields
      AnalysisConfig(
        fields("trackerLists").asJsObject.fields.map {
          case (key, value) => key -> value.asInstanceOf[JsString].value
        },
        fields("honeyDataFirstiPhoneJson").asInstanceOf[JsString].value,
        fields("honeyDataSecondiPhoneJson").asInstanceOf[JsString].value,
        fields("patternJson").asInstanceOf[JsString].value,
        fields("chatterJsonIphone").asInstanceOf[JsString].value,
        fields("chatterJsonAndroid").asInstanceOf[JsString].value,
        fields("screenshotAnalysis").asInstanceOf[JsString].value,
        fields("iphoneAndroidAppNameTranslation").asInstanceOf[JsString].value,
        fields("honeyDataMapping")
          .asInstanceOf[JsArray]
          .elements
          .map { _.convertTo[HoneyDataMapping] }
          .toList,
        fields("dataSets")
          .asInstanceOf[JsArray]
          .elements
          .map {
            _.convertTo[AnalysisDataSet]
          }
          .toList
      )
    }
  }

  def readConfig(file: String): AnalysisConfig = {
    JsonParser(FileInteraction.readFile(file)).convertTo[AnalysisConfig]
  }

}
