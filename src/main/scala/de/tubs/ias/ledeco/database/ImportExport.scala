package de.tubs.ias.ledeco.database

import com.typesafe.config.Config
import de.halcony.argparse.ParsingResult
import de.tubs.ias.ledeco.database.entities.{CellphoneApplication, Collection}
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import wvlet.log.LogSupport

object ImportExport extends LogSupport {

  def getAllApps(id: Int,
                 connection: PostgresConnection): Set[CellphoneApplication] = {
    connection.withSession { implicit session =>
      sql"""SELECT app_name, app_version FROM AppMonitorings WHERE collection = $id"""
        .map(entity =>
          CellphoneApplication(entity.string("app_name"),
                               entity.string("app_version")))
        .list()
        .apply()
        .toSet
    }
  }

  def listCollectionsMain(@annotation.unused pargs: ParsingResult,
                          config: Config): Unit = {
    val mergedDatabase = ImportExport.connectToMergedDatabase(config)
    mergedDatabase.withSession { implicit session =>
      sql"SELECT id, phone, start_time, end_time FROM collections"
        .map { entity =>
          s"${entity.int("id")},${entity.string("phone")},${entity.dateTime(
            "start_time")},${entity.dateTime("end_time")}"
        }
        .list()
        .apply()
        .foreach { str =>
          println(str)
        }
    }
  }

  def connectToMergedDatabase(config: Config): PostgresConnection = {
    val user = config.getString("database.user")
    val dbname = config.getString("database.name")
    val host = config.getString("database.host")
    val port = config.getString("database.port")
    info(s"connecting to internal ${dbname} on ${host}:${port} as $user ")
    new PostgresConnection(
      user,
      config.getString("database.password"),
      config.getString("database.name"),
      config.getString("database.host"),
      config.getString("database.port"),
    )
  }

  def connectToExternalDatabase(pargs: ParsingResult): PostgresConnection = {
    val user = pargs.get[String]("user")
    val host = pargs.get[Option[String]]("host").get
    val port = pargs.get[Option[String]]("port").get
    val dbname = pargs.get[String]("dbname")
    info(s"connecting to external ${dbname} on ${host}:${port} as $user ")
    new PostgresConnection(
      user,
      pargs.get[String]("password"),
      dbname,
      host,
      port
    )
  }

  def importDatabaseAndroidMain(pargs: ParsingResult, config: Config): Unit = {
    val phoneCollection = connectToExternalDatabase(pargs)
    val mergedDatabase = connectToMergedDatabase(config)
    info("reading in the collected intercepts")
    val (collection, _) =
      new AndroidDatabase(phoneCollection).getCollection(NoIdentifyer, None)
    info(
      s"the provided android database contains ${collection.monitoring.size} different monitorings collected from ${collection.start} until ${collection.end}")
    new MergedDataCollection(mergedDatabase).importCollection(collection)
  }

  def importDatabaseIphoneMain(pargs: ParsingResult, config: Config): Unit = {
    val phoneCollection = connectToExternalDatabase(pargs)
    val mergedDatabase = connectToMergedDatabase(config)
    info("reading in the collected intercepts")
    val (collection, _) =
      new IphoneDatabase(phoneCollection).getCollection(NoIdentifyer, None)
    info(
      s"the provided iphone database contains ${collection.monitoring.size} different monitorings collected from ${collection.start} until ${collection.end}")
    new MergedDataCollection(mergedDatabase).importCollection(collection)
  }

  def importRemoteCollectionsMain(pargs: ParsingResult,
                                  config: Config): Unit = {
    info(
      s"starting to transfer collections ${pargs.get[String]("collections")}")
    pargs
      .get[String]("collections")
      .split(",")
      .map(_.toInt)
      .map { collectionID =>
        val remote = new MergedDataCollection(connectToExternalDatabase(pargs))
        val (good, bad) =
          remote.getCollection(NumericCollectionIdentifyer(collectionID), None)
        val local = new MergedDataCollection(connectToMergedDatabase(config))
        val newId = local.importCollection(good, Some(bad))
        s"$collectionID => $newId"
      }
      .foreach(println)
  }

  def getBothPhoneCollections(
      database: PostgresConnection,
      ids: Seq[Int]): ((Collection, Seq[(CellphoneApplication, String)]),
                       (Collection, Seq[(CellphoneApplication, String)])) = {
    ids.toList match {
      case first :: second :: Nil =>
        (new MergedDataCollection(database)
           .getCollection(NumericCollectionIdentifyer(first), None),
         new MergedDataCollection(database)
           .getCollection(NumericCollectionIdentifyer(second), None))
      case only :: Nil =>
        val (good, bad) = new MergedDataCollection(database)
          .getCollection(NumericCollectionIdentifyer(only), None)
        val (lhs, rhs) = good.split()
        ((lhs, bad), (rhs, bad))
      case x =>
        throw new RuntimeException(
          s"you have to provide the ids comma separated, I got $x")
    }
  }
}
