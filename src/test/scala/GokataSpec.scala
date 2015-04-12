import org.scalatest._

class GokataSpec extends FlatSpec {

  val newGoGame = new NewGoGame(5, 5)

  "The move at the start of a game" must "be black" in {
    assert(newGoGame.isLegalMove(Move(0, 0, BlackPiece)))
    assert(!newGoGame.isLegalMove(Move(0, 0, WhitePiece)))
  }

  "The move's position" must "be inside the game board" in {
    assert(!newGoGame.isLegalMove(Move(-1, 2, BlackPiece)))
    assert(!newGoGame.isLegalMove(Move(3, -1, BlackPiece)))
    assert(!newGoGame.isLegalMove(Move(5, 1, BlackPiece)))
    assert(!newGoGame.isLegalMove(Move(4, 5, BlackPiece)))
  }
}