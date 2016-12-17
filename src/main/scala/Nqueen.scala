object Nqueen {

  case class Queen(x: Int, y: Int) // Queen with its position

  type Solution = List[Queen] // List of queens placed in the board
  type Solutions = List[Solution] // List of solutions

  def main(args: Array[String]): Unit = 1 to 10 foreach solveNqueen

  def solveNqueen(size: Int) {
    def placeQueens(n: Int): Solutions = n match {
      case 0 => List(Nil) // cannot be empty
      case _ => for {
        queens <- placeQueens(n - 1) // DFS recursive call, current solution
        y <- 1 to size
        queen = Queen(n, y) // creates a queen for each x,y combination (checks all the board)
        if isSafe(queen, queens) // if the queen is in a safe position
      } yield queen :: queens // it is added to the current solution, yields to Solutions
    }

    val solutions = placeQueens(size)
    println(s"N=$size, ${solutions.size} solutions found")

//    for (currentSol <- solutions) {
//      printSolution(size, currentSol)
//      println()
//    }
  }

  def isSafe(currentQueen: Queen, others: List[Queen]) =
    others forall (!isAttacked(currentQueen, _)) // others don't attack current

  def isAttacked(currentQueen: Queen, otherQueen: Queen) =
    currentQueen.x == otherQueen.x || // attacked in same row
      currentQueen.y == otherQueen.y || // attacked in same column
      (otherQueen.x - currentQueen.x).abs ==
        (otherQueen.y - currentQueen.y).abs // attacked in diagonal

  /*
  print the solutions in the form:
         ---> y
        |
        v
        x
  */
  def printSolution(size: Int, currentSol: Solution): Unit = {
    for (queen <- currentSol; y <- 1 to size) {
      if (queen.y == y) print("Q ") else print(". ")
      if (y == size) println()
    }
  }
}
