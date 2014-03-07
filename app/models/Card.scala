package models

/**
 * @author Marcin Burczak
 * @since 05.03.14
 */
case class Card(color: Color, figure: Figure)

sealed trait Figure
case object Two extends Figure
case object Three extends Figure
case object Four extends Figure
case object Five extends Figure
case object Six extends Figure
case object Seven extends Figure
case object Eight extends Figure
case object Nine extends Figure
case object Ten extends Figure
case object Jack extends Figure
case object Queen extends Figure
case object King extends Figure
case object Ace extends Figure

sealed trait Color
case object Spade extends Color
case object Clube extends Color
case object Diamond extends Color
case object Heart extends Color
