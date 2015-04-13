/**
 * A position on board can be occupied by BlackPiece,
 * WhitePiece, or not occupied yet (Empty)
 */
sealed abstract class Piece {
  val enemyPiece: Piece = Empty
}
case object Empty extends Piece
case object BlackPiece extends Piece {
  override val enemyPiece = WhitePiece
}
case object WhitePiece extends Piece {
  override val enemyPiece = BlackPiece
}

case class Move(val x: Int, val y: Int, val piece: Piece) {
  require(piece != Empty, "Invalid piece, only BlackPiece or WhitePiece")
}

trait GoGameDef {
  val rowCount: Int
  val colCount: Int

  var history: Vector[BoardState] = Vector()

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
    def isOccupiedPos = currentBoardState.positions(move.x)(move.y) != Empty
    def isSelfCapture = {
      val currentPositions = currentBoardState.positions
      val newPositions = currentPositions.updated(move.x, currentPositions(move.x).updated(move.y, move.piece))
      val capturedEnemyPieces = getCapturedPieces(newPositions, move.piece.enemyPiece)
      if (!capturedEnemyPieces.isEmpty) {
        false
      }
      else {
        val capturedOwnPieces = getCapturedPieces(newPositions, move.piece)
        !capturedOwnPieces.isEmpty
      }
    }

    isNextMove && isInsideBoard(move.x, move.y) && !isOccupiedPos && !isSelfCapture
  }

  def playMove(move: Move) = {
    require(isLegalMove(move), "Illegal move")
    val currentPositions = currentBoardState.positions
    val positionsWithMovePiece = currentPositions.updated(move.x, currentPositions(move.x).
      updated(move.y, move.piece))

    history = history :+ BoardState(positionsWithMovePiece, move.piece.enemyPiece)
  }

  /**
   * Check whether a coordinate is inside the board
   */
  protected def isInsideBoard(x: Int, y: Int) = 0 <= x && x < rowCount && 0 <= y && y < colCount

  type Positions = Vector[Vector[Piece]]

  /**
   * Given a board position, get all the captured pieces
   * which are same type of the input piece
   * @param positions
   * @param piece
   * @return A set contains coordinate positions of captured piece
   */
  protected def getCapturedPieces(positions: Positions, piece: Piece): Set[(Int, Int)] = {
    require(piece != Empty)

    val adjacentMoves = List((-1, 0), (1, 0), (0, -1), (0, 1))

    /**
     * Get all connected-pieces components, which is same type of the input piece
     */
    def getConnectedPieces(): List[Set[(Int, Int)]] = {

      val result = scala.collection.mutable.ListBuffer[Set[(Int, Int)]]()

      val visited = scala.collection.mutable.Set[(Int, Int)]()

      for {
        i <- 0 until positions.length
        j <- 0 until positions(i).length
        if positions(i)(j) == piece && !(visited contains (i, j))
      } {
        var connected = Set[(Int, Int)]()

        val queue = scala.collection.mutable.Queue[(Int, Int)]()
        queue += ((i, j))
        visited += ((i, j))

        while (!queue.isEmpty) {
          val pos = queue.dequeue
          connected += pos

          for {
            move <- adjacentMoves
            nextPos = (pos._1 + move._1, pos._2 + move._2)
            if (isInsideBoard(nextPos._1,nextPos._2) && positions(nextPos._1)(nextPos._2) == piece
              && !(visited.contains(nextPos)))
          } {
            queue += nextPos
            visited += nextPos
          }
        }

        result += connected
      }

      result.toList
    }

    /**
     * Whether a connected-pieces component is surrounded by enemy
     */
    def isSurrounded(component: Set[(Int, Int)]): Boolean = {
      for {
        pos <- component
        move <- adjacentMoves
        nextPos = (pos._1 + move._1, pos._2 + move._2)
        if isInsideBoard(nextPos._1, nextPos._2) && positions(nextPos._1)(nextPos._2) == Empty
      } return false

      true
    }

    getConnectedPieces().filter(isSurrounded).flatten.toSet
  }

  /**
   * Board state class holds the state of all positions of the board
   * @param positions
   */
  case class BoardState(val positions: Positions, val nextPiece: Piece = BlackPiece) {
    require(positions.length == rowCount && positions(0).length == colCount, "Invalid positions length")

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
