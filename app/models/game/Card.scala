package models.game

sealed trait Figure {
  def value: Int = 0
}
case object Two extends Figure
case object Three extends Figure
case object Four extends Figure
case object Five extends Figure
case object Six extends Figure
case object Seven extends Figure
case object Eight extends Figure
case object Nine extends Figure
case object Ten extends Figure {
  override def value: Int = 10
}
case object Jack extends Figure {
  override def value: Int = 2
}
case object Queen extends Figure {
  override def value: Int = 3
}
case object King extends Figure {
  override def value: Int = 4
}
case object Ace extends Figure {
  override def value: Int = 11
}

sealed trait Color {
  def trump: Int
}
case object Spade extends Color {
  override def trump: Int = 40
}
case object Clube extends Color {
  override def trump: Int = 60
}
case object Diamond extends Color {
  override def trump: Int = 80
}
case object Heart extends Color {
  override def trump: Int = 100
}

case class Card(color: Color, figure: Figure) {
  def value = figure.value
}
