package de.tubs.ias.ledeco.utility

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StatisticsTest  extends AnyWordSpec with Matchers {

  "ztest" should {
    "return proper value" in {
      val z = Statistics.zTest(23,50,43,70)
      println(z)
      val oz = Statistics.zTest(43,70,23,50)
      println(oz)
    }

  }

}
