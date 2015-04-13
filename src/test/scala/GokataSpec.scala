import org.scalatest._

class GokataSpec extends WordSpec {

  "isLegalMove()" when {

    val newGoGame = new NewGoGame(5, 5)

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
        """o
          |-xx--
          |x-ox-
          |xoox-
          |-xx--
          |-----
        """.stripMargin
      addBoardToHistory
    }

    trait KoRuleGame extends StringParserGoGame {
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

    "at the start of the game" should {
      "be true if black move" in {
        assert(newGoGame.isLegalMove(Move(0, 0, BlackPiece)))
      }

      "be false if white move" in {
        assert(!newGoGame.isLegalMove(Move(0, 0, WhitePiece)))
      }
    }

    "the move is outside the board's coordinate" must {
      "be false" in {
        assert(!newGoGame.isLegalMove(Move(-1, 2, BlackPiece)))
        assert(!newGoGame.isLegalMove(Move(3, -1, BlackPiece)))
        assert(!newGoGame.isLegalMove(Move(5, 1, BlackPiece)))
        assert(!newGoGame.isLegalMove(Move(4, 5, BlackPiece)))
      }
    }

    "the move is on occupied position" must {
      "be false" in {
        new SelfCaptureGame1 {
          assert(!isLegalMove(Move(0, 1, WhitePiece)))
        }
      }
    }

    "the move has no liberties (self-capturing)" must {
      "be false" in {
        new SelfCaptureGame1 {
          assert(!isLegalMove(Move(0, 0, WhitePiece)))
        }
      }
    }

    "the move causes its connected group has no liberties (self-capturing)" must{
      "be false" in {
        new SelfCaptureGame2 {
          assert(!isLegalMove(Move(1, 1, WhitePiece)))
        }
      }
    }

    "the move captures enemy's positions, even its position has no liberties" should {
      "be true" in {
        new KoRuleGame {
          assert(isLegalMove(Move(2, 1, WhitePiece)))
        }
      }
    }
  }

  "getCapturedPieces()" when {
    trait CapturedBoardGoGame extends StringParserGoGame {
      val board =
        """o
          |xxo---
          |ooooo-
          |oxxxoo
          |-xoxx-
          |xoox--
          |-xxx--
        """.stripMargin
      addBoardToHistory
    }

    "piece param is BlackPiece" must {
      "return a set contains captured black pieces" in {
        new CapturedBoardGoGame {
          val capturedPieces = getCapturedPieces(currentBoardState.positions, BlackPiece)
          assert(capturedPieces == Set((0, 0), (0, 1)))
        }
      }
    }

    "piece param is WhitePiece" must {
      "return a set contains capture white pieces" in {
        new CapturedBoardGoGame {
          val capturedPieces = getCapturedPieces(currentBoardState.positions, WhitePiece)
          assert(capturedPieces == Set((3, 2), (4, 1), (4, 2)))
        }
      }
    }
  }
}