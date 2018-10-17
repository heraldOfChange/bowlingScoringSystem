package mh.doodles.scala.bowlingscoringsystem

import scala.util.{Success, Try}

class Foo {

  def peekAhead(data: Seq[Seq[Int]], index: Int) = Try(data(index))

  def isAStrike(pinsKnocked: Seq[Int]): Boolean = pinsKnocked.length == 1 && pinsKnocked.contains(10)

  def isASpare(pinsKnocked: Seq[Int]): Boolean = pinsKnocked.length == 2 && pinsKnocked.sum == 10

  def isLastRound(round: Int): Boolean = round >= 9

  /**
    * look ahead at most two games & tabulate two throws in the event of a strike in the current round
    *
    * @param data the final bowling score playoff
    * @return the total bonus for strikes
    */
  def calculateStrikeBonus(data: Seq[Seq[Int]]): Int =
    data.zipWithIndex.map { case (a, b) =>
      if (isLastRound(b)) 0
      else peekAhead(data, b + 1) match {
        case Success(next) =>
          if (isAStrike(a) && !isAStrike(next)) next.sum
          else if (isAStrike(a) && isAStrike(next)) peekAhead(data, b + 2) match {
            case Success(next2) => next.sum + next2.sum
            case _ => 0
          }
          else 0
        case _ => 0
      }
    }.sum

  /**
    * look ahead one game & tabulate the first throw in the event of a spare in the current round
    *
    * @param data the final bowling score playoff
    * @return the total bonus for spares
    */
  def calculateSpareBonus(data: Seq[Seq[Int]]): Int =
    data.zipWithIndex.map { case (a, b) =>
      if (isLastRound(b)) 0
      else peekAhead(data, b + 1) match {
        case Success(next) => if (isASpare(a)) next.head else 0
        case _ => 0
      }
    }.sum

  /**
    * score the game & add the bonuses from strikes & spares
    *
    * @param data the final bowling score playoff
    * @return the final game score
    */
  def calculateTotal(data: Seq[Seq[Int]]): Int = data.map(_.sum).sum

}
