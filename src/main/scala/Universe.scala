class Universe(val rows:Int, val columns: Int) {

    val matrix: Seq[Seq[Cell]] = (0 until rows).map(row => {
            (0 until columns).map(column => new Cell(row,column))
        })
        
    def getNeighbouringLocations(row: Int, column: Int): List[(Int, Int)] = {
       def f(a: Int) = List(a - 1, a, a + 1)
       val x_locations = f(row).filter(x => {x >= 0 &&  x < rows})
       val y_locations = f(column).filter(y => {y >= 0 && y < columns}) 
       x_locations.flatMap(x => {y_locations.map(y => (x, y))}).filterNot(location => {location._1 == row && location._2 == column})
    }

    def printUniverse() {
        for(row <- 0 until rows) {
            for(column <- 0 until columns) {
                print(" " + matrix(row)(column).value)
            }
            println()
        }
        println()
    }

    def updateAliveNeighborCount() {
        matrix.foreach(row => row.foreach(cell => { 
            cell.aliveNeighbours = getNeighbouringLocations(cell.location.x, cell.location.y).filter(location => {
                matrix(location._1)(location._2).alive == true
            }).size
        }))
    }

    def updateUniverse() {
        updateAliveNeighborCount()
        matrix.foreach(row => row.foreach(cell => {
            if(cell.aliveNeighbours > 3) cell.alive = !cell.alive 
            else if(cell.aliveNeighbours < 2) cell.alive = false
            cell.value = if(cell.alive) "*" else " "
        }))
    }

}

object Universe {

    def apply(rows: Int, columns: Int) = {
        var universe = new Universe(rows, columns)
        universe
    }

   

}