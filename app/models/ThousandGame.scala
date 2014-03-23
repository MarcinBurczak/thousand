package models

import util.Random

/**
 * @author Marcin Burczak
 * @since 05.03.14
 */
object ThousandGame {

  val trump = Map[Color, Int](
    Heart -> 100,
    Diamond -> 80,
    Clube -> 60,
    Spade -> 40)

  val cardValue = Map[Figure, Int](
    Ace -> 11,
    Ten -> 10,
    King -> 4,
    Queen -> 3,
    Jack -> 2,
    Nine -> 0)

  val pack = Seq(
    Card(Heart, Ace),
    Card(Heart, Ten),
    Card(Heart, King),
    Card(Heart, Queen),
    Card(Heart, Jack),
    Card(Heart, Nine),
    Card(Diamond, Ace),
    Card(Diamond, Ten),
    Card(Diamond, King),
    Card(Diamond, Queen),
    Card(Diamond, Jack),
    Card(Diamond, Nine),
    Card(Clube, Ace),
    Card(Clube, Ten),
    Card(Clube, King),
    Card(Clube, Queen),
    Card(Clube, Jack),
    Card(Clube, Nine),
    Card(Spade, Ace),
    Card(Spade, Ten),
    Card(Spade, King),
    Card(Spade, Queen),
    Card(Spade, Jack),
    Card(Spade, Nine))

  def shufflePack() = Random.shuffle(pack)
}
