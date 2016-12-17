import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Cell(label: Char, x: Int, y: Int)

object Pegman {
  def solution(input: String): String = {
    val (height, length, grid) = ("""(\d+)\s(\d)((\n|.)*)""".r.findAllIn(input).matchData map {
      m => (m.group(1).toInt, m.group(2).toInt, m.group(3).replace("\n", "").toCharArray)
    }).next()

    val cellsByRow = new mutable.HashMap[Int, mutable.Set[Cell]] with mutable.MultiMap[Int, Cell]
    val cellsByCol = new mutable.HashMap[Int, mutable.Set[Cell]] with mutable.MultiMap[Int, Cell]
    val unprocessedCells = new ListBuffer[Cell]()

    var counter = 0
    for (row <- 0 until height) {
      for (col <- 0 until length) {
        val currentCell = Cell(grid(counter), col, row)
        print(currentCell.label)
        if ("^v<>".contains(currentCell.label)) {
          cellsByCol.addBinding(col, currentCell)
          cellsByRow.addBinding(row, currentCell)
          unprocessedCells += currentCell
        }
        counter += 1
      }
      println("")
    }

    def pointsUpToCell(col: Int, row: Int) = cellsByCol(col).exists(_.y < row)

    def pointsDownToCell(col: Int, row: Int) = cellsByCol(col).exists(_.y > row)

    def pointsRighToCell(col: Int, row: Int) = cellsByRow(row).exists(_.x > col)

    def pointsLeftToCell(col: Int, row: Int) = cellsByRow(row).exists(_.x < col)

    def canPointToOther(x: Int, y: Int) =
      pointsUpToCell(x, y) || pointsDownToCell(x, y) || pointsRighToCell(x, y) || pointsLeftToCell(x, y)

    def searchPosibleChanges(x: Int, y: Int) =
      if (canPointToOther(x, y))
        1
      else
        throw new scala.Exception("Can't be changed, Pegman dies")

    try {
      val minMov = unprocessedCells.map({
        case Cell('^', x, y) if pointsUpToCell(x, y) => 0
        case Cell('v', x, y) if pointsDownToCell(x, y) => 0
        case Cell('>', x, y) if pointsRighToCell(x, y) => 0
        case Cell('<', x, y) if pointsLeftToCell(x, y) => 0
        case Cell(_, x, y) => searchPosibleChanges(x, y)
      }).sum

      s"$minMov"
    } catch {
      case e: Exception => "IMPOSSIBRU!!"
    }
  }

  def main(args: Array[String]): Unit = {
    println("Case 1: " + solution(
      """2 1
        |^
        |^
      """.stripMargin))
    println("Case 2: " + solution(
      """2 2
        |>v
        |^<
      """.stripMargin))
    println("Case 3: " + solution(
      """3 3
        |...
        |.^.
        |...
      """.stripMargin))
    println("Case 4: " + solution(
      """1 1
        |.
      """.stripMargin))
    println("Case 5: " + solution(
      """1 2
        |<>
      """.stripMargin))
    println("Case 6: " + solution(
      """2 2
        |^>
        |<v
      """.stripMargin))
  }
}