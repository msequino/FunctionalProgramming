package recfun

import scala.annotation.tailrec

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
    def pascal(c: Int, r: Int): Int =
      if(c == 0 || r == c )  1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def iter(head: Char, tail: List[Char], balance: Int) : Boolean = {

        if(head == ')' && balance == 0) false
        else {
          val newBalance = (if(head == '(') balance + 1 else if(head == ')')  balance - 1 else balance)

          if(tail.isEmpty) newBalance == 0
          else iter(tail.head, tail.tail, newBalance)
        }
      }

      iter(chars.head, chars.tail, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      if(money == 0) 1
      else if(coins.isEmpty || money < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)

    }
  }
