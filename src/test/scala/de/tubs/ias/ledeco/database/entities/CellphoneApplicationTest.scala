package de.tubs.ias.ledeco.database.entities

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CellphoneApplicationTest extends AnyWordSpec with Matchers {

  "App.getUniqueElementMap" should {
    "be able to identify the last unique element" in {
      val apps = Set(
        CellphoneApplication("com.app.significant","11"),
        CellphoneApplication("com.other.othersignificant","11")
      )
      CellphoneApplication.getUniqueElementMap(apps) shouldBe Map(
        "significant" -> CellphoneApplication("com.app.significant","11"),
        "othersignificant" -> CellphoneApplication("com.other.othersignificant","11")
      )
    }
  }

}
