package game.bowling

class BowlingGame() {

  var rounds: List[BowlingRound] = Nil

  def completeRound(roundNumber: Int,
                    standardThrows: List[Int],
                    additionalThrows: List[Int] = List.empty) = {

    checkGameConstraints(roundNumber)

    val round = roundNumber match {
      case BowlingGame.MAX_ROUNDS =>
        new FinalBowlingRound(roundNumber, standardThrows, additionalThrows)
      case _ =>
        new NonFinalBowlingRound(roundNumber, standardThrows)
    }

    rounds = round :: rounds
  }

  def checkGameConstraints(roundNumber: Int): Unit = {
    if (roundNumber > BowlingGame.MAX_ROUNDS)
      throw new GameOverException

    if (rounds.exists(r => r.roundNumber == roundNumber))
      throw new RoundAlreadyPlayedException
  }

  def score: Int = rounds.foldLeft(0)((score, round) => {

    val nextRound = rounds.filter(b => b.roundNumber == round.roundNumber + 1)

    if (round.hasStrike)
      score + BowlingGame.SPARE_AND_STRIKE_BASIC_SCORE + calculateAdditionalStrikePoints(round, nextRound)
    else if (round.hasSpare)
      score + BowlingGame.SPARE_AND_STRIKE_BASIC_SCORE + calculateAdditionalSparePoints(round, nextRound)
    else
      score + round.standardThrows.sum
  })

  def calculateAdditionalStrikePoints(round: BowlingRound,
                                      nextRound: List[BowlingRound]): Int =
    round match {
      case FinalBowlingRound(roundNumber, standardThrows, additionalThrows) =>
        additionalThrows.sum
      case NonFinalBowlingRound(roundNumber, standardThrows) =>
        calculateAdditionalNonFinalStrikePoints(nextRound)
    }

  def calculateAdditionalNonFinalStrikePoints(nextRoundList: List[BowlingRound]): Int =
    nextRoundList match {
      case Nil => 0
      case nextRound :: rest => nextRound.standardThrows.sum +
        (rounds.filter(b => b.roundNumber == nextRound.roundNumber + 1) match {
        case Nil => 0
        case followingRound :: rest =>
          if (nextRound.hasTwoStandardThrows) 0
          else followingRound.standardThrows.head
        })
    }

  def calculateAdditionalSparePoints(round: BowlingRound,
                                     nextRoundList: List[BowlingRound]): Int =
    round match {
      case FinalBowlingRound(roundNumber, standardThrows, additionalThrows) =>
        additionalThrows.head
      case NonFinalBowlingRound(roundNumber, standardThrows) =>
        nextRoundList match {
          case Nil => 0
          case nextRound :: rest => nextRound.standardThrows.head
        }
    }
}

object BowlingGame {

  val MAX_ROUNDS = 10
  val SPARE_AND_STRIKE_BASIC_SCORE = 10

  def newGame = new BowlingGame
}
