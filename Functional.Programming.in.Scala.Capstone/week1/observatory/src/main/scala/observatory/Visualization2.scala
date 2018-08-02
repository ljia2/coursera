package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + d01 * (1-point.x)* point.y + d10 * point.x * (1-point.y) + d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val pixels = for(x <- 0 until 256; y <- 0 until 256) yield (x, y)

    val coloredPixels = pixels.par.map{case (x, y) => Interaction.subTile(x, y, tile, 8)}
      .map(subtile => {
        val subLoc = Interaction.tileLocation(subtile)
        val d00 = grid(GridLocation(subLoc.lat.floor.toInt, subLoc.lon.floor.toInt))
        val d01 = grid(GridLocation(subLoc.lat.floor.toInt, subLoc.lon.ceil.toInt))
        val d10 = grid(GridLocation(subLoc.lat.ceil.toInt, subLoc.lon.floor.toInt))
        val d11 = grid(GridLocation(subLoc.lat.ceil.toInt, subLoc.lon.ceil.toInt))

        val color = Visualization.interpolateColor(colors,
          bilinearInterpolation(CellPoint(subLoc.lon - subLoc.lon.floor, subLoc.lat - subLoc.lat.floor), d00, d01, d10, d11))

        Pixel(color.red, color.green, color.blue, 127)

      })
    Image(256, 256, coloredPixels.toArray)
  }

}
