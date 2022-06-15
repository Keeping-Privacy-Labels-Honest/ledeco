package de.tubs.ias.ledeco.database

import scalikejdbc.{
  ConnectionPool,
  ConnectionPoolSettings,
  DB,
  DBSession,
  GlobalSettings,
  LoggingSQLAndTimeSettings,
  using
}

class PostgresConnection(user: String,
                         password: String,
                         dbname: String,
                         host: String,
                         port: String) {

  initialize()

  def initialize(): Unit = {
    GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
      enabled = false)
    val settings = ConnectionPoolSettings(
      initialSize = 100,
      maxSize = scala.concurrent.ExecutionContext.global.##
    )
    val url = s"jdbc:postgresql://$host:$port/$dbname"
    ConnectionPool.add(dbname, url, user, password, settings)
  }

  def withSession[T](func: DBSession => T): T = {
    using(ConnectionPool(dbname).borrow()) { con =>
      DB(con).localTx { session =>
        func.apply(session)
      }
    }
  }

}
