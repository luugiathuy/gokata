/**
 * A position on board can be occupied by BlackPiece,
 * WhitePiece, or not occupied yet (Empty)
 */
sealed abstract class Piece {
  val opponentPiece: Piece = Empty
}
case object Empty extends Piece
case object BlackPiece extends Piece {
  override val opponentPiece = WhitePiece
}
case object WhitePiece extends Piece {
  override val opponentPiece = BlackPiece
}

case class Move(x: Int, y: Int, piece: Piece) {
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
   * @return true if the move is legal, false otherwise
   */
  def isLegalMove(move: Move): Boolean = {
    def isNextMove = move.piece == currentBoardState.nextPiece
    def isOccupiedPos = currentBoardState.positions(move.x)(move.y) != Empty
    def isSelfCapture = {
      val currentPositions = currentBoardState.positions
      val newPositions = currentPositions.updated(move.x, currentPositions(move.x).updated(move.y, move.piece))
      val capturedOpponentPieces = getCapturedPieces(newPositions, move.piece.opponentPiece)
      if (capturedOpponentPieces.nonEmpty) {
        false
      }
      else {
        val capturedOwnPieces = getCapturedPieces(newPositions, move.piece)
        capturedOwnPieces.nonEmpty
      }
    }

    isNextMove && isInsideBoard(move.x, move.y) && !isOccupiedPos && !isSelfCapture
  }

  def playMove(move: Move) = {
    require(isLegalMove(move), "Illegal move")

    // place the move's piece
    val currentPositions = currentBoardState.positions
    val positionsWithPiece = currentPositions.updated(move.x, currentPositions(move.x).
      updated(move.y, move.piece))

    // remove captured opponent's pieces
    val capturedPieces = getCapturedPieces(positionsWithPiece, move.piece.opponentPiece)
    val newPositions = positionsWithPiece.zipWithIndex.map {
      case (rowPieces, i) => rowPieces.zipWithIndex.map {
        case (piece, j) => if (capturedPieces.contains((i, j))) Empty else piece
      }
    }

    history = history :+ BoardState(newPositions, move.piece.opponentPiece)
  }

  /**
   * Check whether a coordinate is inside the board
   */
  protected def isInsideBoard(x: Int, y: Int): Boolean = {
    0 <= x && x < rowCount && 0 <= y && y < colCount
  }

  type Positions = Vector[Vector[Piece]]

  /**
   * Given a board position, get all the captured pieces
   * which are same type of the input piece
   * @return A set contains coordinate positions of captured piece
   */
  protected def getCapturedPieces(positions: Positions, piece: Piece): Set[(Int, Int)] = {
    require(piece != Empty)

    val adjacentDisplacement = List((-1, 0), (1, 0), (0, -1), (0, 1))

    /**
     * Get all connected-pieces components, which is same type of the input piece
     */
    def connectedPieces: List[Set[(Int, Int)]] = {

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

        while (queue.nonEmpty) {
          val pos = queue.dequeue()
          connected += pos

          for {
            delta <- adjacentDisplacement
            nextPos = (pos._1 + delta._1, pos._2 + delta._2)
            if (isInsideBoard(nextPos._1,nextPos._2) && positions(nextPos._1)(nextPos._2) == piece
              && !visited.contains(nextPos))
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
        delta <- adjacentDisplacement
        nextPos = (pos._1 + delta._1, pos._2 + delta._2)
        if isInsideBoard(nextPos._1, nextPos._2) && positions(nextPos._1)(nextPos._2) == Empty
      } return false

      true
    }

    connectedPieces.filter(isSurrounded).flatten.toSet
  }

  /**
   * Board state class holds the state of all positions of the board
   */
  case class BoardState(positions: Positions, nextPiece: Piece = BlackPiece) {
    require(positions.length == rowCount && positions.head.length == colCount, "Invalid positions length")

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
