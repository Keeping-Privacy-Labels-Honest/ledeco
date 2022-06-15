package de.tubs.ias.ledeco.analysis

import de.halcony.ppm.graph.Graph
import de.halcony.ppm.graph.generics.Plot
import de.halcony.ppm.graph.legends.DefaultLegend
import de.halcony.ppm.graph.visual.bar.BarOrientation.horizontal
import de.halcony.ppm.graph.visual.bar.BarPlotAxis
import de.halcony.ppm.style.AxisDenominator.Y

trait BarGraphPlottable {

  def createBarGraph(outFile: String, plots: Seq[Plot], max: Int): Unit = {
    assert(plots.nonEmpty)
    new Graph()
      .addAxis(
        new BarPlotAxis()
          .setBarOrientation(horizontal)
          .setBarWidth(3)
          .setXMin(0)
          .setXMax(1)
          .setHeight(60)
          .enlargeLimits(0.1, Y)
          .addPlots(plots)
          .addLegend(new DefaultLegend())
      )
      .compile(s"$outFile.tex")
  }

}
