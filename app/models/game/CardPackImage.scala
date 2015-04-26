package models.game

object CardPackImage {

  val path = "public/images/ClassicPack.jpg"

  val colorY = Map(
    Heart -> 519,
    Diamond -> 259,
    Clube -> 0,
    Spade -> 778)

  val figureX = Map(
    King -> 2143,
    Queen -> 1965,
    Jack -> 1786,
    Ten -> 1607,
    Nine -> 1429,
    Eight -> 1250,
    Seven -> 1071,
    Six -> 893,
    Five -> 714,
    Four -> 535,
    Three -> 357,
    Two -> 178,
    Ace -> 0)

  val reverseX = 357
  val reverseY = 1036
}
