case class NewGoGame(rowCount: Int, colCount: Int) extends GoGameDef {
  history = history :+ StartBoard
}