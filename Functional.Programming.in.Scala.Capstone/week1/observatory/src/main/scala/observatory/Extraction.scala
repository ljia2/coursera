package observatory

import java.time.LocalDate

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = Source.fromInputStream(getClass.getResourceAsStream(stationsFile))
      .getLines
      .map(_.split(","))
      .filter(fields => fields.length == 4 && !fields(2).equalsIgnoreCase("") && !fields(3).equalsIgnoreCase(""))
      .map(fields => {
        val stn = Try(fields(0).toInt) match {
          case Success(stn) => Some(stn)
          case Failure(_) => None
        }
        val wban = Try(fields(1).toInt) match {
          case Success(wban) => Some(wban)
          case Failure(_) => None
        }
        val location = new Location(fields(2).toDouble, fields(3).toDouble)
        (stn, wban) -> location
      }).toMap

    Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile))
      .getLines()
      .map(_.split(","))
      .filter(fields => fields.length == 5 && fields(2) != "" && fields(3) != "" && fields(4) != "")
      .map(fields => {
        val stn = Try(fields(0).toInt) match {
          case Success(stn) => Some(stn)
          case Failure(_) => None
        }
        val wban = Try(fields(1).toInt) match {
          case Success(wban) => Some(wban)
          case Failure(_) => None
        }
        val month = fields(2).toInt
        val date = fields(3).toInt
        val temperature = (fields(4).toDouble - 32D) / 1.8D
        (LocalDate.of(year, month, date), (stn, wban), temperature)})
      .filter{case (_, loc, _) => stations.contains(loc)}
      .map{case (date, loc, temp) => (date, stations(loc), temp)}.toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy(_._2)
      .mapValues(_.foldLeft((0D, 0))((acc: (Double, Int), elem: (LocalDate, Location, Temperature)) => (acc._1 +  elem._3, acc._2 + 1)))
      .map{case (loc, (sum, count)) => (loc, sum/count)}
  }
}
