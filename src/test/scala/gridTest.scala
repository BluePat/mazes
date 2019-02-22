import org.scalatest.FunSuite

class gridTest extends FunSuite {

  val grid: Grid = Grid(5, 5)

  test("Random cell is within range") {
    val cell: Cell = grid.getRandomCell
    assert(cell.row <= grid.rows)
    assert(cell.column <= grid.columns)
  }
}
