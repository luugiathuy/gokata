/**
 * This trait implements a parser to define board states from a
 * graphical ASCII representation.
 *
 * When mixing in this trait, we can get the `initialBoardRecord`
 * by defining the field `boardASCII` in the following form:
 *
 *   val boardASCII =
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

  val boardASCII: String

  lazy val rowCount = positions.length
  lazy val colCount = positions.head.length

  private lazy val positions: Positions = {
    parseBoardPositions(boardASCII)
  }

  protected lazy val initialBoardRecord: BoardRecord = {
    val newBoard = {
      val nextPiece = if (boardASCII.charAt(0) == 'o') WhitePiece else BlackPiece
      Board(positions, nextPiece)
    }
    Stream(newBoard)
  }

  protected def parseBoardPositions(boardStr: String): Positions = {
    def charToPiece(c: Char) = {
      if (c == 'x') BlackPiece
      else if (c == 'o') WhitePiece
      else Empty
    }
    Vector(boardASCII.split("\n").drop(1).map(str => Vector(str: _*)
      .map(c => charToPiece(c))): _*)
  }
}
