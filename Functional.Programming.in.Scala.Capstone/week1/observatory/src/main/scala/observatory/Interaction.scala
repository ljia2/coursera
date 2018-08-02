package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(lon = tile.x * 360 / pow(2, tile.zoom) - 180, lat = atan(sinh(Pi - tile.y / pow(2, tile.zoom) * 2 * Pi)) * 180 / Pi)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val pixels = for(x <- 0 until 256; y <- 0 until 256) yield (x, y)

    val coloredPixels = pixels.map{case (x, y) => subTile(x, y, tile, 8)}
      .map(subtile => tileLocation(subtile))
      .map(loc => Visualization.predictTemperature(temperatures, loc))
      .map(temp => Visualization.interpolateColor(colors, temp))
      .map(color => Pixel(color.red, color.green, color.blue, 127)).toArray

    Image(256, 256, coloredPixels)
  }


  def subTile(x: Int, y: Int, tile: Tile, zoom: Int): Tile = {
    Tile((x + tile.x * pow(2, zoom)).toInt, (y + tile.y * pow(2, zoom)).toInt, zoom)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
      (year, data) <- yearlyData
    } yield generateImage(year, Tile(x, y, zoom), data)
  }
}
