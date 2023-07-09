package example
import scala.io.Source
import spray.json._
import DefaultJsonProtocol._
import java.io.FileWriter
import org.json4s._
import org.json4s.native.Serialization
import java.io.FileNotFoundException

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
      implicit val formats: DefaultFormats.type = DefaultFormats

      try{
        val locationsFileContents = Source.fromFile(locationsPath).getLines.mkString
        val regionsFileContents = Source.fromFile(regionsPath).getLines.mkString
        if(locationsFileContents.isEmpty || regionsFileContents.isEmpty){
          println("One of the input file is empty")
          System.exit(1)
        }

        val locations = locationsFileContents.parseJson.convertTo[List[Location]]
        val regions = regionsFileContents.parseJson.convertTo[List[Region]]

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

        val json = Serialization.write(finalResults)

        val wd = os.pwd/"results"
        os.remove.all(wd)
        os.makeDir.all(wd)
        os.write(wd/resultsPath, json)

      } catch {
      case _: FileNotFoundException =>
        println("File not found. Please provide valid file paths.")
        System.exit(1)
      case _: DeserializationException =>
        println("Error while parsing JSON. Please ensure the JSON files are correctly formatted.")
        System.exit(1)
      case ex: Exception =>
        println(s"An error occurred: ${ex.getMessage}")
        System.exit(1)
      }
    }
    
    def isLocationInRegion(location: Location, region: Region): Boolean = {
      val point = location.coordinates
      val area = region.coordinates.head

      var isInside = false

      for (i <- 0 until area.length) {
        val j = if (i == 0) area.length - 1 else i - 1

        val xi = area(i)(0)
        val yi = area(i)(1)
        val xj = area(j)(0)
        val yj = area(j)(1)

        if (((yi > point(1)) != (yj > point(1))) 
        && (point.head < (xj - xi) * (point(1) - yi) / (yj - yi) + xi)) {
          isInside = !isInside
        }
      }
      isInside
    }
}
