package de.tubs.ias.ledeco.analysis

import de.tubs.ias.ledeco.database.entities.Collection
import de.tubs.ias.ppm.bargraph._
import de.tubs.ias.ppm.tikzGeneral.{Black, Color, ColorWheel, TikzSVG}
import wvlet.log.LogSupport

import scala.collection.mutable.{Map => MMap}

class AnalysisPlotting(analysis: List[(String, Analysis, Collection)],
                       out: String)
    extends LogSupport {

  val pairs: List[(Color, Analysis, String, Collection)] = {
    val colorWheel = new ColorWheel()
    analysis.map(elem => (colorWheel.getNextColor, elem._2, elem._1, elem._3))
  }

  private def calculateHeight(ticks: Int, labels: Int) = {
    List((ticks * (labels * 1.1 * 5 * 0.03) + (ticks * 1.2)).ceil.toInt, 20).max
  }

  private def trackerAnalysisToPlot(
      subAnalysis: List[Analysis],
      color: Color,
      label: Option[String],
      trackerToBeConsidered: Option[Set[AnalysisData]]): Plot = {
    val trackerCount: MMap[String, Int] = MMap()
    subAnalysis.foreach { single: Analysis =>
      single.getTrackerApps.foreach {
        case (key, value) =>
          if (value.nonEmpty || (trackerToBeConsidered.nonEmpty && trackerToBeConsidered.get
                .contains(key))) {
            if (trackerCount.contains(key.getLabel)) {
              trackerCount(key.getLabel) += value.length
            } else {
              trackerCount.addOne(key.getLabel -> value.length)
            }
          }
      }
    }
    val coordinates = trackerCount.map {
      case (trackerTld, count) => Coordinate(count.toString, trackerTld)
    }
    Plot(color, 0.1, color, coordinates.toList, label)
  }

  def plotTracker(
      trackerToBeConsidered: Option[Set[AnalysisData]] = None): Unit = {
    val texFile = s"$out/trackerAggregate.tex"
    val svgFile = s"$out/trackerAggregate.svg"
    val plot = trackerAnalysisToPlot(analysis.map(_._2),
                                     Black,
                                     None,
                                     trackerToBeConsidered)
    new BarGraph(
      svgFile,
      Axis(BarOrientation.horizontal,
           0.2,
           5,
           height = Some(20),
           xmin = Some(0)),
      List(plot)
    ).createPictureTex(texFile)
    TikzSVG.compile(texFile)
  }

  def plotTrackerByCategories(): Unit = {
    // get all the tracker that for at lest one category are used by at least one app
    val trackerToConsider = pairs
      .flatMap(_._2.getTrackerApps.toList.filter(_._2.nonEmpty).map(_._1))
      .toSet
    val plots = pairs.map {
      case (color, trackerAnalysis, label, _) =>
        trackerAnalysisToPlot(List(trackerAnalysis),
                              color,
                              Some(label),
                              Some(trackerToConsider))
    }
    val texFile = s"$out/trackerByCategory.tex"
    val svgFile = s"$out/trackerByCategory.svg"
    val ticks = plots.map(_.coordinates.length).max
    val labelCount = pairs.length
    val height = calculateHeight(ticks, labelCount)
    info(
      s"to plot all tracker by category I estimate a needed height of ${height}cm")
    new BarGraph(
      svgFile,
      Axis(BarOrientation.horizontal,
           0.2,
           5,
           height = Some(height),
           //height = Some(60),
           xmin = Some(0)),
      plots
    ).createPictureTex(texFile)
    TikzSVG.compile(texFile)
  }

  def plotRequestCountMedian(): Unit = {
    val texFile = s"$out/requestCountMedian.tex"
    val svgFile = s"$out/requestCountMedian.svg"
    val coordinates = pairs.map {
      case (_, _, label, collection) =>
        Coordinate(collection.getRequestCountMedian.toString, label)
    }
    new BarGraph(
      svgFile,
      Axis(BarOrientation.horizontal,
           0.2,
           5,
           height = Some(20),
           xmin = Some(0)),
      List(Plot(Black, 0.1, Black, coordinates))
    ).createPictureTex(texFile)
    TikzSVG.compile(texFile)
  }

  def plotRequestCountMean(): Unit = {
    val texFile = s"$out/requestCountMean.tex"
    val svgFile = s"$out/requestCountMean.svg"
    val coordinates = pairs.map {
      case (_, _, label, collection) =>
        Coordinate(collection.getRequestCountMean.toString, label)
    }
    new BarGraph(
      svgFile,
      Axis(BarOrientation.horizontal,
           0.2,
           5,
           height = Some(20),
           xmin = Some(0)),
      List(Plot(Black, 0.1, Black, coordinates))
    ).createPictureTex(texFile)
    TikzSVG.compile(texFile)
  }

}
