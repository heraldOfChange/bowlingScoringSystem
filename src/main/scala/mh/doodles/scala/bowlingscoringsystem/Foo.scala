package mh.doodles.scala.bowlingscoringsystem

import scala.util.{Failure, Success, Try}

class Foo {

  def peekAhead(data: Seq[Seq[Int]], index: Int) = Try(data(index))

  def isAStrike(pinsKnocked: Seq[Int]): Boolean = pinsKnocked.length == 1 && pinsKnocked.contains(10)

  def isLastRound(round: Int): Boolean = round >= 9

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

  def calculateSpareBonus(data: Seq[Seq[Int]]): Unit = {

  }


  /**
    * returns the calculated bowling score total
    *
    * @param data
    * @return
    */
  def calculateTotal(data: Seq[Seq[Int]]) = data.map(_.sum).sum


}
