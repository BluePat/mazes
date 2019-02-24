import scala.math.abs

case class Cell(row: Int, column: Int, adjacentCells: Map[(Int, Int), Cell], links: List[Cell]) {

  def link(that: Cell, bidirectional: Boolean = true): Cell = {
    if (bidirectional) that.link(this, bidirectional = false)
    this.copy(links = that :: links)
  }

  def unlink(that: Cell, bidirectional: Boolean = true): Cell = {
    if (bidirectional) that.unlink(this, bidirectional = false)
    this.copy(links = links.filter(_ != that))
  }

  def linked(that: Cell): Boolean =
    this.links.contains(that)

}

object Cell {

  def placeCell(row: Int, column: Int, grid: Grid): Cell = {

    def getNeighbour(i: Int, j: Int): Option[Cell] =
      if ((0 to grid.rows contains i) && (0 to grid.columns contains j)) Some(grid.layout(i)(j))
      else None

    val seqSurroundings = for {
      i <- row - 1 to row + 1
      j <- column - 1 to column + 1
      if i != abs(j)
    } yield (i, j) -> getNeighbour(i, j)

    val mappedAdjacentCells: Map[(Int, Int), Cell] = seqSurroundings.toMap map {
      case (coordinates, Some(value)) => coordinates -> value
    }

    Cell(row, column, mappedAdjacentCells , Nil)
  }

}
