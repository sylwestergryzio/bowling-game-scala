package game.bowling

case class BowlingRound(roundNumber: Int, allThrows: List[Int]) {

  def hasStrike = allThrows.head == 10

  def hasSpare = allThrows.head < 10 && allThrows.take(2).sum == 10

  def isFinal = roundNumber == BowlingGame.MAX_ROUNDS
}
