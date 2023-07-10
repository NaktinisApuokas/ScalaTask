package example
import scala.io.Source
import spray.json._
import DefaultJsonProtocol._
import java.io.FileWriter
import org.json4s._
import org.json4s.native.Serialization
import java.io.FileNotFoundException

class InternshipTaskSpec extends munit.FunSuite {
  test("correctly determine if a location is inside a region") {
    val location = Location("location1", (25.2105, 54.6405))
    val region = Region("region1", List(List((25.1357, 54.6792),(25.1561, 54.5847), (25.2866, 54.5942), (25.4294, 54.6461), (25.36416, 54.7710), (25.1357, 54.7710), (25.1357, 54.6792))))

    assertEquals(InternshipTask.isLocationInRegion(location, region), true)
  }

  test("correctly determine if a location is not inside a region") {
    val location = Location("location1", (26.2105, 54.6405))
    val region = Region("region1", List(List((25.1357, 54.6792), (25.1561, 54.5847), (25.2866, 54.5942), (25.4294, 54.6461), (25.36416, 54.7710), (25.1357, 54.7710), (25.1357, 54.6792))))
    
    assertEquals(InternshipTask.isLocationInRegion(location, region), true)
  }

  test("produce correct results when both locations and regions files are empty") {
    val locationsPath = "empty_locations.json"
    val regionsPath = "empty_regions.json"
    val resultsPath = "results.json"

    val expectedResults = List.empty[Results]
    intercept[FileNotFoundException]{
      InternshipTask.main(Array(locationsPath, regionsPath, resultsPath))
    }
      
  }

  test("produce correct results when locations file is empty") {
    val locationsPath = "empty_locations.json"
    val regionsPath = "regions.json"
    val resultsPath = "results.json"

    val expectedResults = List(
      Results("region1", List("location1", "location2")),
      Results("region2", List("location3", "location4", "location5"))
    )

    intercept[FileNotFoundException]{
      InternshipTask.main(Array(locationsPath, regionsPath, resultsPath))
    }
  }

  test("produce correct results when regions file is empty") {
    val locationsPath = "locations.json"
    val regionsPath = "empty_regions.json"
    val resultsPath = "results.json"

    val expectedResults = List.empty[Results]

    intercept[FileNotFoundException]{
      InternshipTask.main(Array(locationsPath, regionsPath, resultsPath))
    }
  }

  test("produce correct results when matching locations with regions") {
    val locations = List(
      Location("location1", (25.2105, 54.6405)),
      Location("location2", (25.2177, 54.6989)),
      Location("location3", (23.8525, 54.8713)),
      Location("location4", (23.9822, 54.8505)),
      Location("location5", (24.0182, 54.9023)),
      Location("location6", (24.4767, 55.1581))
    )
    val regions = List(
      Region("region1", List(List((25.1357, 54.6792), (25.1561, 54.5847), (25.2866, 54.5942), (25.4294, 54.6461), (25.36411, 54.7710), (25.1357, 54.77102), (25.1357, 54.6792)))),
      Region("region2", List(List((23.7284, 54.8580), (23.8345, 54.8157), (24.0262, 54.8157), (24.0711, 54.8721), (24.0425, 54.9846), (23.7284, 54.9846), (23.7284, 54.8580)))),
      Region("region3", List(List((21.0990, 55.6973), (21.1316, 55.6397), (21.2336, 55.6766), (21.2010, 55.7410), (21.1357, 55.8006), (21.0990, 55.6973)), List((21.1007, 55.6445), (21.0855, 55.4883), (20.9763, 55.3074), (21.0491, 55.3195), (21.1159, 55.4952), (21.1310, 55.6308), (21.1007, 55.6445))))
    )
    val expectedResults = List(
      Results("region1", List("location1", "location2")),
      Results("region2", List("location3", "location4", "location5")),
      Results("region3", List[String]())
    )
    val matchingRegions = regions.flatMap { region =>
    locations.filter(InternshipTask.isLocationInRegion(_, region)).map(location => (region.name, location.name))
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

    val calculatedResults = matchedRegions ++ emptyRegions
    assertEquals(calculatedResults, expectedResults)
  }
}