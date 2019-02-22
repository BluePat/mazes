case class Cell(row: Int, column: Int, adjacentCells: List[Option[Cell]], links: List[Cell]) {

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
      if ((0 to grid.rows contains i) && (0 to grid.columns contains j)) Some(grid(i)(j))
      else None

    val seqOfAdjacent = for {
      i <- row -1 to row + 1 by 2
      j <- column -1 to column + 1 by 2
    } yield getNeighbour(i, j)

    Cell(row, column, seqOfAdjacent.toList, Nil)
  }

}
