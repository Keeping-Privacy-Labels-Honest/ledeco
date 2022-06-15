package de.tubs.ias.ledeco.utility

import scala.util.matching.Regex

object UtilityCollection {

  /** split a given url into domain and path
    *
    * @param url the url
    * @return tuple containing first the domain then the path
    */
  def splitUrl(url: String): (String, String) = {
    val httpsWithPath: Regex = """^(https://[^/]*)(/.*)$""".r
    val httpsWithoutPath: Regex = """^(https://[^/]*)$""".r
    val httpWithPath: Regex = """^(http://[^/]*)(/.*)$""".r
    val httpWithoutPath: Regex = """^(http://[^/]*)$""".r
    url match {
      case httpsWithPath(domain, path) => (domain, path)
      case httpWithPath(domain, path)  => (domain, path)
      case httpsWithoutPath(domain)    => (domain, "")
      case httpWithoutPath(domain)     => (domain, "")
    }
  }

}
