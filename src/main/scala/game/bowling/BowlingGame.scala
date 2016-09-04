package game.bowling

import scala.collection.mutable.MutableList

class BowlingGame() {

  val rounds: MutableList[BowlingRound] = MutableList.empty

  def completeRound(roundNumber: Int, throwsList: MutableList[Int]) = {

    if(roundNumber > BowlingGame.MAX_ROUNDS)
      throw new GameOverException

    if(rounds.exists(r => r.roundNumber == roundNumber))
      throw new RoundAlreadyPlayedException

    rounds += new BowlingRound(roundNumber, throwsList.toList)
  }

  def score: Int = {
    // assume rounds were played in order 1-10.

    rounds.foldLeft(0)((score, round) => {

      val nextRound = rounds.filter(b => b.roundNumber == round.roundNumber + 1)
      val nextRound2 = rounds.filter(b => b.roundNumber == round.roundNumber + 2)

      if (round.hasStrike) {
        if(round.isFinal) {
          score + 10 + round.allThrows.takeRight(2).sum
        } else {
          score + 10 + (if(nextRound.size > 0) {
              if(nextRound(0).allThrows.size > 1) nextRound(0).allThrows.take(2).sum
              else {
                nextRound(0).allThrows.sum +
                  (if (nextRound2.size > 0) nextRound2(0).allThrows.head else 0)
              }
            } else 0)
        }
      } else if (round.hasSpare) {
        if(round.isFinal) {
          score + 10 + round.allThrows.last
        } else {
          score + 10 + (if(nextRound.size > 0) nextRound(0).allThrows.head else 0)
        }
      } else {
        score + round.allThrows.sum
      }
    })
  }
}

object BowlingGame {

  val MAX_ROUNDS = 10

  def newGame = new BowlingGame
}
