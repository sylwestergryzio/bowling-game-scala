package game.bowling

import scala.collection.mutable.MutableList

class BowlingGame() {

  val rounds: MutableList[BowlingRound] = MutableList.empty

  def completeRound(roundNumber: Int, throwsList: List[Int]) = {

    if(roundNumber > BowlingGame.MAX_ROUNDS)
      throw new GameOverException

    if(rounds.exists(r => r.roundNumber == roundNumber))
      throw new RoundAlreadyPlayedException

    rounds += new BowlingRound(roundNumber, throwsList)
  }

  def score: Int = rounds.foldLeft(0)((score, round) => {

    val nextRound = rounds.filter(b => b.roundNumber == round.roundNumber + 1)
    val followingNextRound = rounds.filter(b => b.roundNumber == round.roundNumber + 2)

    if (round.hasStrike)
      score + BowlingGame.SPARE_AND_STRIKE_BASIC_SCORE + calculateAdditionalStrikePoints(round, nextRound, followingNextRound)
    else if (round.hasSpare)
      score + BowlingGame.SPARE_AND_STRIKE_BASIC_SCORE + calculateAdditionalSparePoints(round, nextRound)
    else
      score + round.allThrows.sum
  })

  def calculateAdditionalStrikePoints(round: BowlingRound,
                                      nextRound: MutableList[BowlingRound],
                                      followingNextRound: MutableList[BowlingRound]) : Int = {
    if (round.isFinal) round.allThrows.takeRight(2).sum
    else
      if (nextRound.size > 0) {
        if (nextRound(0).allThrows.size > 1)
          nextRound(0).allThrows.take(2).sum
        else {
          nextRound(0).allThrows.sum +
            (if (followingNextRound.size > 0) followingNextRound(0).allThrows.head else 0)
        }
      } else 0
    }

  def calculateAdditionalSparePoints(round: BowlingRound,
                                     nextRound: MutableList[BowlingRound]): Int = {
    if (round.isFinal) round.allThrows.last
    else if (nextRound.size > 0) nextRound(0).allThrows.head else 0
  }
}

object BowlingGame {

  val MAX_ROUNDS = 10

  val SPARE_AND_STRIKE_BASIC_SCORE = 10

  def newGame = new BowlingGame
}
