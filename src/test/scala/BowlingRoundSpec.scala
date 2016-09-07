import game.bowling.{FinalBowlingRound, NonFinalBowlingRound}
import org.scalatest.{Matchers, WordSpec}

class BowlingRoundSpec extends WordSpec with Matchers {

  "A bowling round" when {
    "a player knocked 10 pins down with one throw" should {

      val bowlingRound = new NonFinalBowlingRound(1, List(10))

      "have a strike" in {
        bowlingRound.hasStrike should be(true)
      }

      "not have a spare" in {
        bowlingRound.hasSpare should be(false)
      }
    }

    "a player knocked 10 pins down with two throws" should {

      val bowlingRound = new NonFinalBowlingRound(1, List(4, 6))

      "have a spare" in {
        bowlingRound.hasSpare should be(true)
      }

      "not have a strike" in {
        bowlingRound.hasStrike should be(false)
      }
    }
  }

  "a player throws two times" should {
    "have two standard throws" in {
      val bowlingRound = new NonFinalBowlingRound(1, List(4, 6))
      bowlingRound.hasTwoStandardThrows should be(true)
    }
  }

  "A final bowling round" when {
    "a player threw a strike" should {
      "allow two additional throws" in {
        val bowlingRound = new FinalBowlingRound(1, List(10), List(5, 6))
        bowlingRound.additionalThrows.length should be(2)
      }
    }

    "a player threw a spare" should {
      "allow one additional throw" in {
        val bowlingRound = new FinalBowlingRound(1, List(5, 5), List(5))
        bowlingRound.additionalThrows.length should be(1)
      }
    }
  }
}
