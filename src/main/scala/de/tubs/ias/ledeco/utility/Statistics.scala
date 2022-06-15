package de.tubs.ias.ledeco.utility

object Statistics {

  def statisticalAnalysis(number: Seq[Int]): (Double, Double) = {
    val avg = number.sum.toDouble / number.length.toDouble
    val sigma = Math.sqrt(
      number
        .map(nb => (nb.toDouble - avg) * (nb.toDouble - avg))
        .sum / number.length)
    (avg, sigma)
  }

  //assuming H0: p1 - p2 = 0
  //assuming normal distribution
  def zTest(outlierCount: Int,
            outlierSample: Int,
            restCount: Int,
            restSample: Int): Double = {
    val outlierProportion
      : Double = outlierCount.toDouble / outlierSample.toDouble
    val restProportion: Double = restCount.toDouble / restSample.toDouble
    val pooledProportion
      : Double = (outlierCount + restCount) / (outlierSample + restSample).toDouble
    val proportionDelta: Double = outlierProportion - restProportion
    val pooledProportionCo = pooledProportion * (1 - pooledProportion)
    val weirdSum = (pooledProportionCo / outlierSample.toDouble) + (pooledProportionCo / restSample.toDouble)
    val stdError: Double = Math.sqrt(weirdSum)
    val assumedDelta: Double = 0
    val z: Double = (proportionDelta - assumedDelta) / stdError
    z
  }

}
