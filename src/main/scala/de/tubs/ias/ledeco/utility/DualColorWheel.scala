package de.tubs.ias.ledeco.utility

import de.halcony.ppm.colors.{Color, ColorWheel, White}

class DualColorWheel() {
  private var counter = 0
  private val colorWheelFill = new ColorWheel()
  private val colorWheelBorder = new ColorWheel()

  def getNextColors: (Color, Color) = {
    if (counter < 15) {
      val color = colorWheelFill.getNextColor
      counter += 1
      (color, color)
    } else {
      counter += 1
      (White, colorWheelBorder.getNextColor)
    }
  }
}
