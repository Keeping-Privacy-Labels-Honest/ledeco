package de.tubs.ias.ledeco.database.entities

case class Parameters(cookie: Map[String, String],
                      url: Map[String, String],
                      header: Map[String, String],
                      body: Map[String, String])

object Parameters {

  private def mapMerge(lhs: Map[String, String],
                       rhs: Map[String, String],
                       aggregate: Boolean): Map[String, String] = {
    (lhs.keys ++ rhs.keys).map { key: String =>
      val lhsOption: Option[String] = lhs.get(key)
      val rhsOption: Option[String] = rhs.get(key)
      val merge = (lhsOption, rhsOption) match {
        case (Some(x), Some(y)) if x == y              => x
        case (Some(x), Some(y)) if x != y && aggregate => s"$x$y"
        case (Some(x), Some(y)) if x != y && !aggregate =>
          throw new RuntimeException(
            "same parameter different value no aggregate")
        case (None, Some(x)) => x
        case (Some(x), None) => x
        case (None, None) =>
          throw new RuntimeException("double none should never happen")
      }
      key -> merge
    }.toMap
  }

  def merge(lhs: Parameters,
            rhs: Parameters,
            aggregateMerge: Boolean = true): Parameters = {
    Parameters(
      mapMerge(lhs.cookie, rhs.cookie, aggregateMerge),
      mapMerge(lhs.url, rhs.url, aggregateMerge),
      mapMerge(lhs.body, rhs.body, aggregateMerge),
      mapMerge(lhs.body, rhs.body, aggregateMerge)
    )
  }

}
