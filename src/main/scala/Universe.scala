class Universe {

    var rows: Int = 0
    var columns: Int = 0
    var matrix: Array[Array[Cell]] = null

}

object Universe {

    def apply(rows: Int, columns: Int) = {
        var universe = new Universe
        universe.rows = rows
        universe.columns = columns
        universe.matrix = Array.ofDim[Cell](rows, columns)
        initUniverse(universe)
        universe
    }

     def initUniverse(universe: Universe) {
        var row, column = 0
        for(row <- 0 until universe.rows) {
            for(column <- 0 until universe.columns) {
                universe.matrix(row)(column) = new Cell(row, column)
            }
        }
    }

    def printUniverse(universe: Universe) {
        for(row <- 0 until universe.rows) {
            for(column <- 0 until universe.columns) {
                print(universe.matrix(row)(column).value)
                print(" ")
            }
            println()
        }
    }

    def getCell(universe: Universe, row: Int, column: Int): Cell = {
        universe.matrix(row)(column)
    }

    def updateUniverse(universe: Universe) {
        updateAliveNeighbourCount(universe)
        for(row <- 0 until universe.rows) {
            for(column <- 0 until universe.columns) {
                var cell = getCell(universe, row, column)
                cell.alive = (cell.alive && cell.aliveNeighbours == 2) || (cell.aliveNeighbours == 3) 
                cell.value = if(cell.alive) "*" else " "
            }
        }
    }

    def updateAliveNeighbourCount(universe: Universe) {
        for(row <- 0 until universe.rows) {
            for(column <- 0 until universe.columns) {
                var liveCount = 0
                var cell = getCell(universe, row, column)
                cell.getNeighbours(universe, cell).foreach(neighbour => {
                    if(neighbour.alive) liveCount = liveCount + 1
                })
                cell.aliveNeighbours = liveCount       
            }
        }
    }

}