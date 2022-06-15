package de.tubs.ias.ledeco.utility

import de.halcony.ppm.colors.{Black, Green, Red}
import de.halcony.ppm.graph.generics.AxisAlignment.{CENTER, LEFT}
import de.halcony.ppm.graph.{Coordinate, Graph}
import de.halcony.ppm.graph.visual.bar.{
  BarOrientation,
  BarPlot,
  BarPlotAxis,
  NodesNearCoords
}
import de.halcony.ppm.style.Anchor.{EAST, WEST}
import de.halcony.ppm.style.FontSizes.TINY
import de.halcony.ppm.style.Style
import wvlet.log.LogSupport

object KeynessPlot {

  /** one degree of freedom chi square test
    *
    * @param lhsCount the count of the first category
    * @param lhsOverall  the overall size of the first category
    * @param rhsCount the count of the second category
    * @param rhsOverall the overall size of the second category
    * @return
    */
  def chiSquare(lhsCount: Int,
                lhsOverall: Int,
                rhsCount: Int,
                rhsOverall: Int): Double = {
    val proportion
      : Double = (lhsCount + rhsCount).toDouble / (lhsOverall + rhsOverall).toDouble
    val lhsExpected = proportion * lhsOverall.toDouble
    val rhsExpected = proportion * rhsOverall.toDouble
    val lhsTerm = Math.pow(lhsCount.toDouble - lhsExpected, 2) / lhsExpected
    val rhsTerm = Math.pow(rhsCount.toDouble - rhsExpected, 2) / rhsExpected
    lhsTerm + rhsTerm
  }
}

class KeynessPlot(lhsName: String,
                  lhsSize: Int,
                  lhs: Map[String, Int],
                  rhsName: String,
                  rhsSize: Int,
                  rhs: Map[String, Int])
    extends LogSupport {

  assert(lhs.keySet == rhs.keySet)

  def generateKeynessGraph: Graph = {
    val coordinates = lhs.keySet
      .map { category =>
        val lhsCount = lhs(category)
        val rhsCount = rhs(category)
        val chiSqr = KeynessPlot.chiSquare(lhsCount, lhsSize, rhsCount, rhsSize)
        val lhsRatio = lhsCount.toDouble / lhsSize.toDouble
        assert(
          lhsRatio <= 1,
          s"the ratio cannot be larger than one ($lhsRatio/$lhsSize/$category)")
        val rhsRatio = rhsCount.toDouble / rhsSize.toDouble
        assert(
          rhsRatio <= 1,
          s"the ratio cannot be larger than one ($rhsRatio/$rhsSize/$category)")
        if (lhsRatio < rhsRatio) {
          ("smaller",
           Coordinate((-1 * chiSqr).toString,
                      category.replace('_', '-').replace(' ', '-')))
        } else {
          ("bigger",
           Coordinate(chiSqr.toString,
                      category.replace('_', '-').replace(' ', '-')))
        }
      }
      .groupBy(_._1)
    val bigger = coordinates
      .getOrElse("bigger", List())
      .map(_._2)
      .toList
      .sortBy(_.x.toDouble)
    val smaller = coordinates
      .getOrElse("smaller", List())
      .map(_._2)
      .toList
      .sortBy(_.x.toDouble)
    val axis = new BarPlotAxis()
      .setBarOrientation(BarOrientation.horizontal)
      .setBarWidth(4)
      .setBarShift(0)
      .setXAxisAlignment(LEFT)
      .setYAxisAlignment(CENTER)
      .setYTicksSpacing(4)
      .setNoYTickLabels()
      .enlargeLimits(0.5)
      .disableYTicks()
      .disableXArrowTip()
      .disableYArrowTip()
    if (smaller.nonEmpty) {
      axis.addPlot(
        new BarPlot()
          .setFillColor(Red)
          .setLineColor(Black)
          .addData(smaller)
          .setName(rhsName)
          .setNodesNearCoords(new NodesNearCoords()
            .setStyle(new Style().setFontSize(TINY).setAnchor(EAST))
            .setDataColumn(1))
      )
    }
    if (bigger.nonEmpty) {
      axis.addPlot(
        new BarPlot()
          .setFillColor(Green)
          .setLineColor(Black)
          .addData(bigger)
          .setName(lhsName)
          .setNodesNearCoords(new NodesNearCoords()
            .setStyle(new Style().setFontSize(TINY).setAnchor(WEST))
            .setDataColumn(1))
      )
    }
    new Graph()
      .addAxis(
        axis.disableYTicks().disableYArrowTip().disableYArrowTip()
      )
  }

}
