/**
 * This trait implements a parser to define board states from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a Go Game can be defined by
 * defining the field `board` in the following form:
 *
 *   val board =
 *     """x
 *       |-----
 *       |--o--
 *       |--x--
 *       |-----""".stripMargin
 *
 * - The first line will have one character `x` or `o` denotes
 *    the next piece
 * - The following lines represent the board's position
 * - The `-` character denotes empty positions
 * - `x` denotes positions which are occupied by black
 * - `o` denotes positions which are occupied by white
 *
 */
trait StringParserGoGame extends GoGameDef {

  val board: String

  private lazy val positions: Vector[Vector[Piece]] = {
    parseBoard(board)
  }

  lazy val rowCount = positions.length
  lazy val colCount = positions.head.length

  def addBoardToHistory = {
    history = {
      val nextPiece = if (board.charAt(0) == 'o') WhitePiece else BlackPiece
      Vector(BoardState(positions, nextPiece))
    }
    true
  }

  protected def parseBoard(boardStr: String): Vector[Vector[Piece]] = {
    def charToPiece(c: Char) = {
      if (c == 'x') BlackPiece
      else if (c == 'o') WhitePiece
      else Empty
    }
    Vector(board.split("\n").drop(1).map(str => Vector(str: _*).map(c => charToPiece(c))): _*)
  }

}
