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
    var count = 0

    for(e <- chars){
      if(e == '(') count += 1
      if(e == ')') count -= 1
      if(count <0) return false
    }
    if(count == 0)  true
    else false

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, lp: Int, rp: Int) :(Int, Int) = {
      var l = 0
      var r = 0
      var i = idx
      while (i < until){

        if(chars(idx) == '(') l += 1
        if(chars(idx) == ')') r += 1
        i += 1
      }
      (l,r)
    }


    def reduce(from: Int, until: Int): (Int,Int)= {

      val size = until - from
      if (size > threshold){

       val half = size/2
        val ((l1, r1),(l2, r2)) = parallel(reduce(from, from+half), reduce(until-half, until))
        ((l1 + l2) - (r1+r2), (r1+r2)-(l1 + l2))
      }else
        traverse(from, until, 0, 0)
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
