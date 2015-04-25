package models.game

import org.specs2.mutable.Specification

class CardPackSpec extends Specification {

  "GamePack" should {
    "be shuffle" in {
      val shuffle: Seq[Card] = CardPack.shuffle()

      shuffle must_!= CardPack.pack
    }

    "be sorted" in {
      val sortedShuffle: Seq[Card] = CardPack.shuffle().sorted.reverse

      sortedShuffle must_== CardPack.pack
    }
  }
}
