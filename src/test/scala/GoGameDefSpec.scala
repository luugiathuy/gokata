import org.scalatest._

class GoGameDefSpec extends WordSpec {

  trait NewGoGame5x5 extends GoGameDef {
    val rowCount = 5
    val colCount = 5
    val initialBoardRecord = Stream(Board())
  }

  "isLegalMove()" when {
    "at the start of the game" should {
      "be true if black's move" in {
        new NewGoGame5x5 {
          isLegalMove(Move(0, 0, BlackPiece), initialBoardRecord)
        }
      }

      "be false if white's move" in {
        new NewGoGame5x5 {
          assert(!isLegalMove(Move(0, 0, WhitePiece), initialBoardRecord))
        }
      }
    }

    "the move is outside the board's coordinate" must {
      "be false" in {
        new NewGoGame5x5 {
          assert(!isLegalMove(Move(-1, 2, BlackPiece), initialBoardRecord))
          assert(!isLegalMove(Move(3, -1, BlackPiece), initialBoardRecord))
          assert(!isLegalMove(Move(5, 1, BlackPiece), initialBoardRecord))
          assert(!isLegalMove(Move(4, 5, BlackPiece), initialBoardRecord))
        }
      }
    }

    trait SelfCaptureGame1 extends StringParserGoGame {
      val boardASCII =
        """o
          |-x---
          |x----
          |-----
          |-----
          |-----
        """.stripMargin
    }

    "the move is on occupied position" must {
      "be false" in {
        new SelfCaptureGame1 {
          assert(!isLegalMove(Move(0, 1, WhitePiece), initialBoardRecord))
        }
      }
    }

    "the move has no liberties (self-capturing)" must {
      "be false" in {
        new SelfCaptureGame1 {
          assert(!isLegalMove(Move(0, 0, WhitePiece), initialBoardRecord))
        }
      }
    }

    trait SelfCaptureGame2 extends StringParserGoGame {
      val boardASCII =
        """o
          |-xx--
          |x-ox-
          |xoox-
          |-xx--
          |-----
        """.stripMargin
    }

    "the move causes its connected group has no liberties (self-capturing)" must {
      "be false" in {
        new SelfCaptureGame2 {
          assert(!isLegalMove(Move(1, 1, WhitePiece), initialBoardRecord))
        }
      }
    }

    trait KoRuleGame extends StringParserGoGame {
      val boardASCII =
        """o
          |-----
          |-xo--
          |x-xo-
          |-xo--
          |-----
        """.stripMargin
    }

    "the move captures enemy's positions, even its position has no liberties" should {
      "be true" in {
        new KoRuleGame {
          assert(isLegalMove(Move(2, 1, WhitePiece), initialBoardRecord))
        }
      }
    }

    "the move causes the board state return to previous position" must {
      "be false" in {
        new KoRuleGame {
          val newBoardRecord = playMove(Move(2, 1, WhitePiece), initialBoardRecord)
          assert(!isLegalMove(Move(2, 2, BlackPiece), newBoardRecord))
        }
      }
    }
  }

  "getCapturedPieces()" when {
    trait CapturedBoardGoGame extends StringParserGoGame {
      val boardASCII =
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
          val capturedPieces = getCapturedPieces(parseBoardPositions(boardASCII), BlackPiece)
          assert(capturedPieces == Set((0, 0), (0, 1)))
        }
      }
    }

    "piece param is WhitePiece" must {
      "return a set contains capture white pieces" in {
        new CapturedBoardGoGame {
          val capturedPieces = getCapturedPieces(parseBoardPositions(boardASCII), WhitePiece)
          assert(capturedPieces == Set((3, 2), (4, 1), (4, 2)))
        }
      }
    }
  }

  "playMove()" when {
    "the move is illegal" must {
      "throw IllegalArgumentException" in {
        intercept[IllegalArgumentException] {
          new NewGoGame5x5 {
            playMove(Move(0, 0, WhitePiece), initialBoardRecord)
          }
        }
      }
    }

    "the move is legal" must {
      "place the piece to board's position" in {
        new NewGoGame5x5 {
          val newBoardRecord = playMove(Move(2, 3, BlackPiece), initialBoardRecord)
          assert(newBoardRecord.head.positions(2)(3) == BlackPiece)
        }
      }

      "change the next piece for next move" in {
        new NewGoGame5x5 {
          val newBoardRecord = playMove(Move(2, 3, BlackPiece), initialBoardRecord)
          assert(newBoardRecord.head.nextPiece == WhitePiece)
        }
      }

      "add new board state to history" in {
        new NewGoGame5x5 {
          val newBoardRecord = playMove(Move(2, 3, BlackPiece), initialBoardRecord)
          assert(newBoardRecord.size == 2)
        }
      }

      "capture opponent pieces if they have no liberties" in {
        trait CapturedBoardGoGame extends StringParserGoGame {
          val boardASCII =
            """x
              |xx----
              |ooooo-
              |oxxxoo
              |-xoxx-
              |-oox--
              |-xxx--
            """.stripMargin
        }

        new CapturedBoardGoGame {
          val newBoardRecord = playMove(Move(4, 0, BlackPiece), initialBoardRecord)
          assert(newBoardRecord.head.positions.flatten.count(p => p == WhitePiece) == 8)
          assert(newBoardRecord.head.positions.flatten.count(p => p == BlackPiece) == 13)
        }
      }
    }
  }
}