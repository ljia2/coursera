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
  ) withWarmer(new Warmer.Default)

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
    var acc = 0
    var index = 0
    while(index < chars.length){
      chars(index) match {
        case '(' => acc = acc + 1
        case ')' => acc = acc - 1
        case _ =>
      }
      if(acc < 0) return false
      index = index + 1
    }
    acc == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leftUnmatched: Int, rightUnmatched: Int): (Int, Int) = {
      var index = idx
      var newLeftCnt = leftUnmatched
      var newRightCnt = rightUnmatched
      while(index < until){
        chars(index) match {
          case '(' => newLeftCnt = newLeftCnt + 1
          case ')' => if(newLeftCnt > 0) newLeftCnt = newLeftCnt - 1 else newRightCnt = newRightCnt + 1
          case _ =>
        }
        index = index + 1
      }
      (newLeftCnt, newRightCnt)
    }

    def reduce(from: Int, until: Int): (Int, Int) /*: ???*/ = {
      if(from < until){
        if(until - from <= threshold) {
          traverse(from, until, 0, 0)
        } else {
          val mid = from + (until - from) / 2
          val ((ll, lr), (rl, rr)) = parallel(reduce(from, mid),
            reduce(mid, until))
          (rl + math.max(0, (ll - rr)), lr + math.max(0, (rr - ll)))
        }
      } else
        (0, 0)
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
