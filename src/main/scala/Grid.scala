case class Grid(rows: Int, columns: Int) {

  type Grid = Vector[Vector[Cell]]
  val grid: Grid = prepareGrid

  private def prepareGrid: Grid =
    Vector.tabulate(this.rows, this.columns){
      (i, j) => Cell(i, j, Nil)
    }

}
