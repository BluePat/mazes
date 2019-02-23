import scala.util.Random

object binaryTreeGenerator {

  val directions: List[(Int, Int)] = List((-1, 0), (0, 1))

  private val random = new Random()

  private def chooseAdjecentCell(cell: Cell): Cell = {
    val direction: (Int, Int) = directions(random.nextInt(1))
    cell.adjacentCells(direction)
  }

  def apply(grid: Grid): Grid = ???

}
