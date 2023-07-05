package example
import spray.json._
import DefaultJsonProtocol._
import org.json4s._
import org.json4s.native.Serialization


class InternshipTaskSpec extends munit.FunSuite {
  test("correctly determine if a location is inside a region") {
    val location = Location("location1", List(25.2105, 54.6405))
      val region = Region("region1", List(List(List(25.1357, 54.6792), List(25.1561, 54.5847), List(25.2866, 54.5942), List(25.4294, 54.6461), List(25.36416, 54.7710), List(25.1357, 54.7710), List(25.1357, 54.6792))))

    assertEquals(InternshipTask.isLocationInRegion(location, region), true)
  }
  test("produce correct results when matching locations with regions") {
    val locations = List(
      Location("location1", List(25.2105, 54.6405)),
      Location("location2", List(25.2177, 54.6989)),
      Location("location3", List(23.8525, 54.8713)),
      Location("location4", List(23.9822, 54.8505)),
      Location("location5", List(24.0182, 54.9023)),
      Location("location6", List(24.4767, 55.1581))
    )
    val regions = List(
      Region("region1", List(List(List(25.1357, 54.6792), List(25.1561, 54.5847), List(25.2866, 54.5942), List(25.4294, 54.6461), List(25.36411, 54.7710), List(25.1357, 54.77102), List(25.1357, 54.6792)))),
      Region("region2", List(List(List(23.7284, 54.8580), List(23.8345, 54.8157), List(24.0262, 54.8157), List(24.0711, 54.8721), List(24.0425, 54.9846), List(23.7284, 54.9846), List(23.7284, 54.8580)))),
      Region("region3", List(List(List(21.0990, 55.6973), List(21.1316, 55.6397), List(21.2336, 55.6766), List(21.2010, 55.7410), List(21.1357, 55.8006), List(21.0990, 55.6973)), List(List(21.1007, 55.6445), List(21.0855, 55.4883), List(20.9763, 55.3074), List(21.0491, 55.3195), List(21.1159, 55.4952), List(21.1310, 55.6308), List(21.1007, 55.6445))))
    )
    val expectedResults = List(
      Results("region1", List("location1", "location2")),
      Results("region2", List("location3", "location4", "location5"))
    )
    val matchingLocations = locations.flatMap { location =>
    regions.find(InternshipTask.isLocationInRegion(location, _)).map(region => (location.name, region.name))
    }

    val calculatedResults = matchingLocations
        .groupBy(_._2)
        .map { case (region, locations) => Results(region, locations.map(_._1)) }
        .toList
    assertEquals(calculatedResults, expectedResults)
  }
}