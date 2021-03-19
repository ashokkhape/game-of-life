class Cell(var row: Int, var column: Int) {
    
    var value: String = "*"
    var alive: Boolean = true
    var aliveNeighbours: Int = 0
    val location: Location = new Location(row, column)

    def isCornerCell(universe: Universe, cell: Cell): Boolean = 
        (cell.location.x == 0 || cell.location.x == universe.rows - 1) && (cell.location.y == 0 || cell.location.y == universe.columns - 1)
    

    def isEdgeCell(universe: Universe, cell: Cell): Boolean =
        cell.location.x == 0 || cell.location.x == universe.rows - 1 || cell.location.y == 0 || cell.location.y == universe.columns - 1
    

    def whichCorner(universe: Universe, cell: Cell): String = {
        if(cell.location.x == 0 && cell.location.y == 0) "topLeft"
        else if(cell.location.x == universe.rows -1 && cell.location.y == 0) "bottomLeft"
        else if(cell.location.x == 0 && cell.location.y == universe.columns - 1) "topRight"
        else "bottomRight"
    }

    def whichEdge(universe: Universe, cell: Cell): String = {
        if(cell.location.x == 0) "top"
        else if(cell.location.x == universe.rows -1) "bottom"
        else if(cell.location.y == 0) "left"
        else "right"
    }

    def getNeighbours(universe: Universe, cell: Cell) = {
        var matrix = universe.matrix
        var x = cell.location.x
        var y = cell.location.y
        if(isCornerCell(universe, cell)) {
            whichCorner(universe, cell) match {
                case "topLeft" => Array(matrix(x+1)(y), matrix(x)(y+1), matrix(x+1)(y+1))
                case "bottomLeft" => Array(matrix(x-1)(y), matrix(x)(y+1), matrix(x)(y+1))
                case "topRight" => Array(matrix(x)(y-1), matrix(x+1)(y-1), matrix(x+1)(y))
                case "bottomRight" => Array(matrix(x)(y-1), matrix(x-1)(y), matrix(x-1)(y-1))
            }
        }
        else if(isEdgeCell(universe, cell)) {
            whichEdge(universe, cell) match {
                case "top" => Array(matrix(x)(y-1), matrix(x)(y+1), matrix(x+1)(y-1), matrix(x+1)(y), matrix(x+1)(y+1))
                case "bottom" => Array(matrix(x)(y-1), matrix(x)(y+1), matrix(x-1)(y-1), matrix(x-1)(y), matrix(x-1)(y+1))
                case "left" => Array(matrix(x-1)(y), matrix(x+1)(y), matrix(x-1)(y+1), matrix(x)(y+1), matrix(x+1)(y+1))
                case "right" => Array(matrix(x-1)(y-1), matrix(x)(y-1), matrix(x+1)(y-1), matrix(x-1)(y), matrix(x+1)(y))
            }
        }
        else {
            Array(matrix(x-1)(y-1), matrix(x-1)(y), matrix(x-1)(y+1), matrix(x)(y-1), matrix(x)(y+1), matrix(x+1)(y-1), matrix(x+1)(y), matrix(x+1)(y+1))
        }
    }

}