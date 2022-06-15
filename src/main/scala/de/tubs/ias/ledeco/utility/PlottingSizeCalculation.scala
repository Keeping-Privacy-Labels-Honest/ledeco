package de.tubs.ias.ledeco.utility

object PlottingSizeCalculation {

  def calculateHeight(ticks: Int, labels: Int): Int = {
    List((ticks * (labels * 1.1 * 5 * 0.03) + (ticks * 1.2)).ceil.toInt, 20).max
  }

}
