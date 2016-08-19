package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def isFirst(): Boolean = c == 0
    def isLast(): Boolean = c == r + 1
    def factorial(n: Int): Int = {
      def fact(acc: Int, n: Int): Int = {
        if (n == 0) acc
        else fact(acc * n, n - 1)
      }
      fact(1, n)
    }

    if (isFirst || isLast) 1
    else factorial(r) / (factorial(c) * factorial(r - c))
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def check(chars: List[Char], counter: Int): Int = {
        if (chars.isEmpty) counter
        else if (chars.head == '(') check(chars.tail, counter + 1)
        else if (chars.head == ')') check(chars.tail, counter - 1)
        else check(chars.tail, counter)
      }

      if (chars.isEmpty) true
      else if (chars.head == ')') false
      else if (chars.last == '(') false
      else check(chars, 0) == 0
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.length == 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
  }
