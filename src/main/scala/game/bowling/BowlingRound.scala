package game.bowling

abstract class BowlingRound {
  def roundNumber: Int

  def standardThrows: List[Int]

  def hasStrike = standardThrows.head == 10

  def hasSpare = standardThrows.head < 10 && standardThrows.take(2).sum == 10

  def hasTwoStandardThrows = standardThrows.size == 2
}

case class NonFinalBowlingRound(roundNumber: Int,
                                standardThrows: List[Int]) extends BowlingRound {}

case class FinalBowlingRound(roundNumber: Int,
                             standardThrows: List[Int],
                             additionalThrows: List[Int]) extends BowlingRound {}