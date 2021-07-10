package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = {
      if n == 0 then 1
      else Range(1, n + 1, 1).reduce(_ * _)
    }
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceLoop(cs: List[Char], acc: Int): Boolean = {
      if acc < 0 then
        false 
      else if cs.isEmpty then
        acc == 0
      else {
        cs.head match {
          case '(' => balanceLoop(cs.tail, acc + 1)
          case ')' => balanceLoop(cs.tail, acc - 1)
          case _ => balanceLoop(cs.tail, acc)
        }
      }
    }
    balanceLoop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if money < 0 then 0
    else if coins.isEmpty then 
      if money == 0 then 1 else 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
