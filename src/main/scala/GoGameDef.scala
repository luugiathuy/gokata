/**
 * A position on board can be occupied by BlackPiece,
 * WhitePiece, or not occupied yet (Empty)
 */
sealed abstract class Piece
case object Empty extends Piece
case object BlackPiece extends Piece
case object WhitePiece extends Piece

case class Move(val x: Int, val y: Int, val piece: Piece)

trait GoGameDef {
  val rowCount: Int
  val colCount: Int

  val history: Vector[BoardState]

  def currentBoardState: BoardState = history.last

  /**
   * Check whether a move is legal.
   * 1. Black places the first stone, after which White and Black alternate.
   * 2. Stones cannot be placed on occupied points.
   * 3. No self-capture. Stones cannot be placed where they would be immediately captured
   * 4. If placing a stone causes an opponent's stone to be captured, the opponents stones are
   *    removed from the board before the liberties of your stone(s) are calculated
   * 5. You cannot place a stone to put the game back in the same position as it was
   *    on your last turn (this prevents infinite loops in play)
   * @param move
   * @return true if the move is legal, false otherwise
   */
  def isLegalMove(move: Move): Boolean = {
    def isNextMove = move.piece == currentBoardState.nextPiece
    def isInsideBoard = 0 <= move.x && move.x < rowCount && 0 <= move.y && move.y < colCount
    def isOccupiedPos = currentBoardState.positions(move.x)(move.y) != Empty

    isNextMove && isInsideBoard && !isOccupiedPos
  }

  type Positions = Vector[Vector[Piece]]

  /**
   * Board state class holds the state of all positions of the board
   * @param positions
   */
  case class BoardState(val positions: Positions, val nextPiece: Piece = BlackPiece) {
    require(positions.length == rowCount && positions(0).length == colCount)
    require(nextPiece != Empty)

    /**
     * Constructor to create new game's board
     */
    def this() = this(Vector.fill(rowCount, colCount)(Empty))
  }

  /**
   * The start board state where all positions are empty
   */
  object StartBoard extends BoardState
}
