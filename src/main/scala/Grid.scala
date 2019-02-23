import scala.util.Random

case class Grid(rows: Int, columns: Int) {

  type Grid = Vector[Vector[Cell]]

  val size: Int = rows * columns
  val layout: Grid = prepareGrid

  private def prepareGrid: Grid =
    Vector.tabulate(this.rows, this.columns){
      (i, j) => Cell.placeCell(i, j, this)
    }

  def getRandomCell: Cell = {
    val random = new Random()
    this.layout(random.nextInt(rows))(random.nextInt(columns))
  }

}
