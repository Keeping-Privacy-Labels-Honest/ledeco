package de.tubs.ias.ledeco.utility

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UtilityCollectionTest extends AnyWordSpec with Matchers {

  "split url" should {
    "give me only the domain for https://facebook.com" in {
      val domain = "https://facebook.com"
      UtilityCollection.splitUrl(domain) shouldBe ("https://facebook.com","")
    }
    "give me the domain and the path for https://facebook.com/path" in {
      val domain = "https://facebook.com/path"
      UtilityCollection.splitUrl(domain) shouldBe ("https://facebook.com","/path")
    }
    "give me the domain and the full path for https://facebook.com/very/long/example/path.php" in {
      val domain = "https://facebook.com/very/long/example/path.php"
      UtilityCollection.splitUrl(domain) shouldBe ("https://facebook.com", "/very/long/example/path.php")
    }
  }

 }
