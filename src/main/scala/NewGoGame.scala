case class NewGoGame(val rowCount: Int, val colCount: Int) extends GoGameDef {
  val history = Vector(StartBoard)
}


