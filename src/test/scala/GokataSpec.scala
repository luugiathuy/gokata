import org.scalatest._

class GokataSpec extends FlatSpec {

  val newGoGame = new NewGoGame(5, 5)

  "The move at the start of the game" must "be black" in {
    assert(newGoGame.isLegalMove(Move(0, 0, BlackPiece)))
    assert(!newGoGame.isLegalMove(Move(0, 0, WhitePiece)))
  }
}