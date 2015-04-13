import org.scalatest._

class GokataSpec extends WordSpec {

  "isLegalMove()" when {
    val newGoGame = new NewGoGame(5, 5)

    "at the start of the game" should {
      "be true if black's move" in {
        assert(newGoGame.isLegalMove(Move(0, 0, BlackPiece)))
      }

      "be false if white's move" in {
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

    "the move causes its connected group has no liberties (self-capturing)" must {
      "be false" in {
        new SelfCaptureGame2 {
          assert(!isLegalMove(Move(1, 1, WhitePiece)))
        }
      }
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

    "the move captures enemy's positions, even its position has no liberties" should {
      "be true" in {
        new KoRuleGame {
          assert(isLegalMove(Move(2, 1, WhitePiece)))
        }
      }
    }

    "the move causes the board state return to previous position" must {
      "be false" in {
        new KoRuleGame {
          playMove(Move(2, 1, WhitePiece))
          assert(!isLegalMove(Move(2, 2, BlackPiece)))
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

    }

    "piece param is BlackPiece" must {
      "return a set contains captured black pieces" in {
        new CapturedBoardGoGame {
          val capturedPieces = getCapturedPieces(parseBoard(board), BlackPiece)
          assert(capturedPieces == Set((0, 0), (0, 1)))
        }
      }
    }

    "piece param is WhitePiece" must {
      "return a set contains capture white pieces" in {
        new CapturedBoardGoGame {
          val capturedPieces = getCapturedPieces(parseBoard(board), WhitePiece)
          assert(capturedPieces == Set((3, 2), (4, 1), (4, 2)))
        }
      }
    }
  }

  "playMove()" when {
    "the move is illegal" must {
      "throws IllegalArgumentException" in {
        intercept[IllegalArgumentException] {
          NewGoGame(5, 5).playMove(Move(0, 0, WhitePiece))
        }
      }
    }

    "the move is legal" must {
      "places the piece to board's position" in {
        val game = NewGoGame(5, 5)
        game.playMove(Move(2, 3, BlackPiece))
        assert(game.currentBoardState.positions(2)(3) == BlackPiece)
      }

      "changes the next piece for next move" in {
        val game = NewGoGame(5, 5)
        game.playMove(Move(2, 3, BlackPiece))
        assert(game.currentBoardState.nextPiece == WhitePiece)
      }

      "adds new board state to history" in {
        val game = NewGoGame(5, 5)
        game.playMove(Move(2, 3, BlackPiece))
        assert(game.history.size == 2)
      }

      "captures opponent pieces if they have no liberties" in {
        trait CapturedBoardGoGame extends StringParserGoGame {
          val board =
            """x
              |xx----
              |ooooo-
              |oxxxoo
              |-xoxx-
              |-oox--
              |-xxx--
            """.stripMargin
          addBoardToHistory
        }

        new CapturedBoardGoGame {
          playMove(Move(4, 0, BlackPiece))
          assert(currentBoardState.positions.flatten.count(p => p == WhitePiece) == 8)
          assert(currentBoardState.positions.flatten.count(p => p == BlackPiece) == 13)
        }
      }
    }
  }
}