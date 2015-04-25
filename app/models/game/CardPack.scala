package models.game

import scala.util.Random

object CardPack {

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

  def shuffle(): Seq[Card] = Random.shuffle(pack)
}
