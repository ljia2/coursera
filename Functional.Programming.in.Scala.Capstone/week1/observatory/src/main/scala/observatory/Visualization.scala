package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import math.abs
/**
  * 2nd milestone: basic visualization
  */
object Visualization {
   val p = 6
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    idw(temperatures, location)
  }

  def idw(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distTempPairs = temperatures.map{case (loc, temp) => (loc.greatCircleDistance(location), temp)}
    val (minDistance, minTemp) = distTempPairs.minBy(_._1)
    if(minDistance < 1D)
      minTemp
    else {
      val idwTemps = distTempPairs.map{case (dist, temp) => (1/math.pow(dist, p), temp)}
      val weightedTempSum = idwTemps.foldLeft(0D)((acc, pair) => acc + pair._1 * pair._2)
      val weightSum = idwTemps.foldLeft(0D)((acc, pair) => acc + pair._1)
      if(weightSum > 0)
        weightedTempSum / weightSum
      else
        0D
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def interpolation(color1: Color, color2: Color, ratio: Double): Color = {
      new Color(color1.red  + ((color2.red - color1.red) * ratio).toInt,
        color1.green + ((color2.green - color1.green) * ratio).toInt,
        color1.blue + ((color2.blue - color1.blue) * ratio).toInt)
    }

    points.filter(_._1 == value).headOption match {
      case Some(p) => p._2
      case None => {
        val sortedPoints = points.toSeq.sortBy(_._1)
        val pointPairs = for {
          index <- 0 until sortedPoints.size-1
          if value >= sortedPoints(index)._1 && value < sortedPoints(index+1)._1
        } yield (sortedPoints(index), sortedPoints(index+1))

        if(pointPairs.isEmpty){
          if(value < sortedPoints.head._1)
            sortedPoints.head._2
          else
            sortedPoints.last._2
        } else {
          val ((temp1, color1), (temp2, color2)) = pointPairs.head
          val ratio = (value - temp1) / (temp2 - temp1)
          interpolation(color1, color2, ratio)
        }
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val pixels = for(x <- 0 until 360; y <- 0 until 180) yield (x - 180, y - 90)
    val coloredPixels = pixels.par.map{ case (lon, lat) => interpolateColor(colors, predictTemperature(temperatures, Location(lat, lon)))}
      .map(color => Pixel(color.red, color.green, color.blue, 255))
      .toArray
    Image(360, 180, coloredPixels)
  }

}

