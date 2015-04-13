case class NewGoGame(val rowCount: Int, val colCount: Int) extends GoGameDef {
  history = history :+ StartBoard
}