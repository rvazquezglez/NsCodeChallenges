object TruckTour {

  case class Pump(petrol: Int, distanceToNext: Int)

  def main(args: Array[String]): Unit = {
    println(solveTruckTour(
      io.Source.stdin.getLines.drop(1)
        .map(_.split(" "))
        .map(x => Pump(x(0).toInt, x(1).toInt))
        .toVector))
  }

  def solveTruckTour(pumps: Vector[Pump]): Int = {
    var start = 0
    var end = 1

    var currentPetrol = pumps.head.petrol - pumps.head.distanceToNext

    while (end != start || currentPetrol < 0) {
      while (currentPetrol < 0 && start != end) {
        currentPetrol -= pumps(start).petrol - pumps(start).distanceToNext
        start = (start + 1) % pumps.size
        if (start == 0) return -1
      }
      currentPetrol += pumps(end).petrol - pumps(end).distanceToNext
      end = (end + 1) % pumps.size
    }
    start
  }
}
