object Main extends App {
  
  val universe: Universe = Universe(3, 3) 

  def isUniverseAlive = {
    universe.matrix.flatMap(row => row.filter(cell => cell.alive)).size > 0
  }
 
 while(isUniverseAlive) {
   universe.printUniverse()
   universe.updateUniverse()
 }
  
}
