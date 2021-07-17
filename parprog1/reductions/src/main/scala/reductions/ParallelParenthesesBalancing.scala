package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceLoop(chars: Array[Char], accum: Int): Boolean = {
      if accum < 0 then false
      else if chars.isEmpty then (accum == 0)
      else chars.head match {
        case '(' => balanceLoop(chars.tail, accum + 1)
        case ')' => balanceLoop(chars.tail, accum - 1)
        case _ => balanceLoop(chars.tail, accum)
      }
    }
    balanceLoop(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if idx == until then
        (arg1, arg2)
      else {
        val newArg1 = arg1 + chars(idx) match {
          case '(' => 1
          case ')' => -1
          case _   => 0
        }
        val newArg2 = arg2 min newArg1
        traverse(idx+1, until, newArg1, newArg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if until - from <= threshold then {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) / 2
        val (left, right) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        (left._1 + right._1, left._2 min (left._1 + right._2))
      }
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

