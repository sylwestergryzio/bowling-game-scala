import game.bowling.{BowlingGame, GameOverException, RoundAlreadyPlayedException}
import org.scalatest._

class BowlingGameSpec extends WordSpec with Matchers {

  "The score" when {

    "9 pins are knocked down during each of the first 2 rounds" should {
      "equal to 18 points" in {
        val game = BowlingGame.newGame
        game.completeRound(1, List(4, 5))
        game.completeRound(2, List(3, 6))

        game.score should be (18)
      }
    }

    "a spare is thrown in a round" should {
      "be higher than the previous score by 10 points + points from one following throw" in {
        val game = BowlingGame.newGame
        game.completeRound(1, List(5, 4))
        game.completeRound(2, List(2, 7))

        // score is 18
        val score = game.score

        game.completeRound(3, List(5, 5)) // 10 + 4 = 14
        game.completeRound(4, List(4, 1)) // 5

        game.score should be (score + 19) // 37
      }
    }

    "a strike is thrown in a round" should {
      "be higher than the previous score by 10 + points from next 2 throws" in {
        val game = BowlingGame.newGame
        game.completeRound(1, List(5, 4))
        game.completeRound(2, List(2, 7))

        // score is 18
        val score = game.score

        game.completeRound(3, List(10)) // 10 + 4 + 3 = 17
        game.completeRound(4, List(4, 3)) // 7

        game.score should be (score + 24) // 42
      }
    }
  }

  "An attempt to complete 11th round" should {
    "throw GameOverException" in {
      val game = BowlingGame.newGame
      an[GameOverException] should be thrownBy game.completeRound(11, List(4, 5))
    }
  }

  "An attempt to complete a round that has already been completed" should {
    "throw RoundAlreadyPlayedException" in {
      val game = BowlingGame.newGame
      game.completeRound(1, List(2, 3))
      an[RoundAlreadyPlayedException] should be thrownBy game.completeRound(1, List(4, 5))
    }
  }

  "A game" when {
    "all rounds are finished" should {
      "tell the exact score with spare in the final round" in {
        val game = BowlingGame.newGame
        game.completeRound(1, List(10))
        game.completeRound(2, List(9, 1))
        game.completeRound(3, List(5, 5))
        game.completeRound(4, List(7, 2))
        game.completeRound(5, List(10))
        game.completeRound(6, List(10))
        game.completeRound(7, List(10))
        game.completeRound(8, List(9, 0))
        game.completeRound(9, List(8, 2))
        game.completeRound(10, List(9, 1), List(10))

        game.score should be (187)
      }

      "tell the exact score with strike in the final round" in {
        val game = BowlingGame.newGame
        game.completeRound(1, List(7, 2))
        game.completeRound(2, List(10))
        game.completeRound(3, List(10))
        game.completeRound(4, List(10))
        game.completeRound(5, List(10))
        game.completeRound(6, List(7, 3))
        game.completeRound(7, List(10))
        game.completeRound(8, List(10))
        game.completeRound(9, List(9, 1))
        game.completeRound(10, List(10), List(10, 9))

        game.score should be (234)
      }
    }
  }
}
