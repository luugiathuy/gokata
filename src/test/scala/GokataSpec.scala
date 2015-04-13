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

  object SelfCaptureGame extends StringParserGoGame {
    val board =
      """o
        |-x---
        |x----
        |-----
        |-----
        |-----
      """.stripMargin
    addBoardToHistory
  }
  
  "The move" must "not be at occupied position" in {
    val game = SelfCaptureGame
    assert(!game.isLegalMove(Move(0, 1, WhitePiece)))
  }

  "The move" must "not be placed where it would be immediately captured" in {
    val game = SelfCaptureGame
    assert(!game.isLegalMove(Move(0, 0, WhitePiece)))
  }

  object NonSelfCaptureGame extends StringParserGoGame {
    val board =
      """o
        |-----
        |-xo--
        |x-xo-
        |-xo--
        |-----
      """.stripMargin
    addBoardToHistory
  }

  "The move" can "be placed where it capture enemy's positions, even its position has no liberty" in {
    val game = NonSelfCaptureGame
    assert(game.isLegalMove(Move(2, 1, WhitePiece)))
  }
}