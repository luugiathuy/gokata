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

  trait SelfCaptureGame1 extends StringParserGoGame {
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

  trait SelfCaptureGame2 extends StringParserGoGame {
    val board =
      """
        |o
        |-xx--
        |x-ox-
        |xoox-
        |-xx--
        |-----
      """.stripMargin
    addBoardToHistory
  }
  
  "The move" must "not be at occupied position" in {
    new SelfCaptureGame1 {
      assert(!isLegalMove(Move(0, 1, WhitePiece)))
    }
  }

  "The move" must "not be placed where it would be immediately captured" in {
    new SelfCaptureGame1 {
      assert(!isLegalMove(Move(0, 0, WhitePiece)))
    }
    new SelfCaptureGame2 {
      assert(!isLegalMove(Move(1, 1, WhitePiece)))
    }
  }

  trait NonSelfCaptureGame extends StringParserGoGame {
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

  "The move" can "be placed where it captures enemy's positions, even its position has no liberty" in {
    new NonSelfCaptureGame {
      assert(isLegalMove(Move(2, 1, WhitePiece)))
    }
  }
}