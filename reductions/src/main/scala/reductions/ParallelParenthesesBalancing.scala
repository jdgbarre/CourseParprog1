package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    //???
    def loop(acc: Int, listChar: List[Char]): Boolean = {
      if (listChar isEmpty) acc == 0
      else if (listChar.head equals '(') loop(acc + 1, listChar.tail)
      else if (listChar.head equals ')') acc > 0 && loop(acc - 1, listChar.tail)
      else loop(acc, listChar.tail)
    }

    loop(0, chars.toList)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      //???
      def loop(acc: (Int, Int), listChar: List[Char]): (Int, Int) = {
        if (listChar isEmpty) acc
        else if (listChar.head equals '(') loop((acc._1 + 1, acc._2), listChar.tail)
        else if (listChar.head equals ')') {
          if (acc._1 > 0) loop((acc._1 - 1, acc._2), listChar.tail)
          else loop((acc._1, acc._2 + 1), listChar.tail)
        }
        else loop(acc, listChar.tail)
      }

      loop((arg1, arg2), chars.toList.slice(idx, until))
    }

    def reduce(from: Int, until: Int): (Int, Int) /*: ???*/ = {
      //???
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel[(Int, Int), (Int, Int)](
          reduce(from, mid),
          reduce(mid, until)
        )

        (left._1 + right._1, left._2 + right._2)
      }
    }

    reduce(0, chars.length) == (0,0)//???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
