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
  require(piece != Empty, "Invalid Piece, only BlackPiece or WhitePiece")
}

trait GoGameDef {

  val rowCount: Int
  val colCount: Int

  type Positions = Vector[Vector[Piece]]

  /**
   * Board class holds the state of all positions of the board
   */
  case class Board(positions: Positions = Vector.fill(rowCount, colCount)(Empty),
                   nextPiece: Piece = BlackPiece) {
    require(positions.length == rowCount && positions.head.length == colCount,
      "Invalid positions length")
  }

  /**
   * A record of boards of the game.
   * The Stream.head is the current board
   */
  type BoardRecord = Stream[Board]

  /**
   * Given a move and board record, check whether a move is legal.
   * 1. Black places the first stone, after which White and Black alternate.
   * 2. Stones cannot be placed on occupied points.
   * 3. No self-capture. Stones cannot be placed where they would be
   *    immediately captured (Suicide rule)
   * 4. If placing a stone causes an opponent's stone to be captured,
   *    the opponents stones are removed from the board before the liberties
   *    of your stone(s) are calculated
   * 5. You cannot place a stone to put the game back in the same position as it was
   *    on your last turn (this prevents infinite loops in play) (ko rule)
   * @return true if the move is legal, false otherwise
   */
  def isLegalMove(move: Move, boardRecord: BoardRecord): Boolean = {
    require(boardRecord.nonEmpty)
    val currentBoard = boardRecord.head

    def isNextMove = move.piece == currentBoard.nextPiece
    def isOccupiedPos = currentBoard.positions(move.x)(move.y) != Empty
    def isSelfCapture = {
      val canCaptureOpponent = getCapturedPieces(positionsWithMovePiece, move.piece.opponentPiece).nonEmpty
      if (canCaptureOpponent) false
      else {
        // if there is any captured piece of move.piece -> self-capture move
        val capturedOwnPieces = getCapturedPieces(positionsWithMovePiece, move.piece)
        capturedOwnPieces.nonEmpty
      }
    }

    def isSameAsPreviousState: Boolean = boardRecord match {
      case (current#::previous#::xs) =>
        val newPosition = removeCapturedPieces(positionsWithMovePiece, move.piece.opponentPiece)
        previous.positions == newPosition
      case _ => false
    }

    lazy val positionsWithMovePiece = positionsWhenPlaceMove(move, currentBoard.positions)

    isNextMove && isInsideBoard(move.x, move.y) && !isOccupiedPos &&
      !isSelfCapture && !isSameAsPreviousState
  }

  /**
   * Given a move and board record, play a move on the board, capture no-liberty opponent's pieces
   * If a move is illegal, an IllegalArgumentException exception will be thrown
   * @return a new board record
   */
  def playMove(move: Move, boardRecord: BoardRecord): BoardRecord = {
    require(boardRecord.nonEmpty)
    require(isLegalMove(move, boardRecord), "Illegal Move")
    val currentBoard = boardRecord.head
    val positionsWithMovePiece = positionsWhenPlaceMove(move, currentBoard.positions)
    val newPositions = removeCapturedPieces(positionsWithMovePiece, move.piece.opponentPiece)
    Board(newPositions, move.piece.opponentPiece) #:: boardRecord
  }

  /**
   * Given a move and position, returns new board's positions by
   * simply placing the move's piece on its position
   */
  private def positionsWhenPlaceMove(move: Move, positions: Positions): Positions = {
    positions.updated(move.x, positions(move.x).updated(move.y, move.piece))
  }

  /**
   * Given a board position, and a type of piece (BlackPiece/WhitePiece),
   * remove the captured pieces of that type from the positions
   * @return the new positions after remove captured pieces
   */
  private def removeCapturedPieces(positions: Positions, piece: Piece): Positions = {
    require(piece != Empty)
    val capturedPieces = getCapturedPieces(positions, piece)
    positions.zipWithIndex.map {
      case (rowPieces, i) => rowPieces.zipWithIndex.map {
        case (p, j) => if (capturedPieces.contains((i, j))) Empty else p
      }
    }
  }

  /**
   * Check whether a coordinate is inside the board
   */
  protected def isInsideBoard(x: Int, y: Int): Boolean = {
    0 <= x && x < rowCount && 0 <= y && y < colCount
  }


  /**
   * Given a board position, get all the captured pieces
   * which are same type of the input piece
   * @return A set contains positions of captured piece
   */
  protected def getCapturedPieces(positions: Positions, piece: Piece): Set[(Int, Int)] = {
    require(piece != Empty)

    /**
     * Given a position (x, y), returns list of its neighbor positions
     */
    def neighbors(x: Int, y: Int): List[(Int, Int)] = {
      val neighborDisplacement = List((-1, 0), (1, 0), (0, -1), (0, 1))
      neighborDisplacement.map({ case (dx, dy)  => (x + dx, y + dy)})
        .filter({ case (i, j) => isInsideBoard(i, j) })
    }

    /**
     * Get all groups which are same type of the input piece
     */
    def getGroups: List[Set[(Int, Int)]] = {

      /**
       * Find the group where piece at (x, y) belongs to
       */
      def findGroup(x: Int, y: Int) : Set[(Int, Int)] = {
        def visit(toVisit: Seq[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = {
          if (toVisit.isEmpty) Set.empty
          else {
            val pos = toVisit.head
            val toVisitFriendNeighbors = neighbors(pos._1, pos._2).toSeq.filter {
              case(i, j) => !visited.contains((i, j)) && positions(i)(j) == positions(pos._1)(pos._2)
            }
            Set((pos._1, pos._2)) ++ visit(toVisit.tail ++ toVisitFriendNeighbors, visited + pos)
          }
        }

        visit(Seq((x, y)), Set.empty)
      }

      def iterPos(x: Int, y: Int, visited: Set[(Int, Int)]): List[Set[(Int, Int)]] = {
        def nextPos(x: Int, y: Int): (Int, Int) = {
          if (y < colCount - 1) (x, y + 1)
          else if (x < rowCount - 1) (x + 1, 0)
          else null
        }

        val group = {
          if (positions(x)(y) == piece && !visited.contains((x, y))) findGroup(x, y)
          else Set.empty[(Int, Int)]
        }

        val next = nextPos(x, y)
        if (next != null) List(group) ++ iterPos(next._1, next._2, visited ++ group)
        else List(group)
      }

      iterPos(0, 0, Set.empty)
    }

    /**
     * Whether a group is surrounded by enemy
     */
    def isSurrounded(group: Set[(Int, Int)]): Boolean = {
      def hasEmptyNeighbor(x: Int, y: Int): Boolean = {
        neighbors(x, y) exists {
          case (i, j) => positions(i)(j) == Empty }
      }
      !(group exists { case(x, y) => hasEmptyNeighbor(x, y) })
    }

    getGroups.filter(isSurrounded).flatten.toSet
  }
}
