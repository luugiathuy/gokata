case class NewGoGame(val rowCount: Int, val colCount: Int) extends GoGameDef {

  val currentBoard = StartBoard
  val nextPiece = BlackPiece
}
