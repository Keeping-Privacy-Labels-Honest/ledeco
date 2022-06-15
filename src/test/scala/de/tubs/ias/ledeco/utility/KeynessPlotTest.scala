package de.tubs.ias.ledeco.utility

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KeynessPlotTest  extends AnyWordSpec with Matchers {

  "chi-sqr"  should {
    "return a small value" in {
      KeynessPlot.chiSquare(70,100,100,200) shouldBe 4.705882352941177
    }
  }

  "keyness plot" should {
    "create a propper plot" in {
      val lhsName = "apple"
      val lhsSize = 100
      val lhsMap = Map(
        "ri-pe" -> 70,
        "sweet" -> 49,
        "orange" -> 30,
        "cthulhu" -> 10
      )
      val rhsName = "rest"
      val rhsSize = 200
      val rhsMap = Map(
        "ri-pe" -> 100,
        "sweet" -> 80,
        "orange" -> 100,
        "cthulhu" -> 90
      )
      new KeynessPlot(lhsName, lhsSize, lhsMap, rhsName, rhsSize, rhsMap)
        .generateKeynessGraph
        .compile("./resources/unittests/trivial.tex") shouldBe true
    }
  }

}
