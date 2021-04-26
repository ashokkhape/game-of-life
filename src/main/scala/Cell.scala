import scala.util.Random

class Cell(var row: Int, var column: Int) {

    var alive: Boolean = Random.nextBoolean()
    var value: String = if(alive) "*" else " "
    var aliveNeighbours: Int = 0
    val location: Location = new Location(row, column)
}