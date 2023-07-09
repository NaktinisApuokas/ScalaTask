package example
import scala.io.Source
import upickle.default._
import spray.json._
import DefaultJsonProtocol._
import java.io.FileWriter
import org.json4s._
import org.json4s.native.Serialization

case class Config(regionsFile: String = "", locationsFile: String = "")
case class Location(name: String, coordinates: List[Double])
case class Region(name: String, coordinates: List[List[List[Double]]])
case class Results(region: String, matchedLocations: List[String])

object InternshipTask {
  def main(args: Array[String])
    {
      if (args.length < 3) {
        println("Please provide paths to the locations and regions JSON files.")
        System.exit(1)
      }
      val locationsPath = args(0)
      val regionsPath = args(1)
      val resultsPath = args(2)

      implicit val locationFormat: RootJsonFormat[Location] = jsonFormat2(Location)
      implicit val regionFormat: RootJsonFormat[Region] = jsonFormat2(Region)

      val locationsJsonString = os.read(os.pwd/locationsPath)
      var locationsData = locationsJsonString.stripMargin
      val locations = locationsData.parseJson.convertTo[List[Location]]

      val regionsJsonString = os.read(os.pwd/regionsPath)
      var regionsData = regionsJsonString.stripMargin
      val regions = regionsData.parseJson.convertTo[List[Region]]

      val matchingRegions = regions.flatMap { region =>
      locations.filter(isLocationInRegion(_, region)).map(location => (region.name, location.name))
      } 

      val matchedRegions = matchingRegions
        .groupBy(_._1)
        .map { case (region, locations) => Results(region, locations.map(_._2)) }
        .toList

      val emptyRegions = regions.flatMap { region =>
        if (!matchingRegions.exists(_._1 == region.name)) {
          Some(Results(region.name, List[String]()))
        } else {
          None
        }
      }

      val finalResults = matchedRegions ++ emptyRegions

      implicit val formats: DefaultFormats.type = DefaultFormats

      val json = Serialization.write(finalResults)

      val wd = os.pwd/"results"
      os.remove.all(wd)
      os.makeDir.all(wd)
      os.write(wd/resultsPath, json)
    }
    
    def isLocationInRegion(location: Location, region: Region): Boolean = {
      val point = location.coordinates
      val area = region.coordinates.head

      def isCoordinatesInArea(x: Double, y: Double): Boolean = {
        var isInside = false
        var i = 0
        var j = area.length - 1

        while (i < area.length) {
          val xi = area(i)(0)
          val yi = area(i)(1)
          val xj = area(j)(0)
          val yj = area(j)(1)

          val intersect = ((yi > y) != (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
          if (intersect) isInside = !isInside

          j = i
          i += 1
        }

        isInside
      }

    isCoordinatesInArea(point.head, point(1))
  }
}
