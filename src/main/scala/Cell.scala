case class Cell(row: Int, column: Int, links: List[Cell]) {

  def link(that: Cell, bidirectional: Boolean = true): Cell = {
    if (bidirectional) that.link(this, bidirectional = false)
    Cell(this.row, this.column, that :: links)
  }

  def unlink(that: Cell, bidirectional: Boolean = true): Cell = {
    if (bidirectional) that.unlink(this, bidirectional = false)
    Cell(this.row, this.column, links.filter(_ != that))
  }

  def linked(that: Cell): Boolean =
    this.links.contains(that)

}
