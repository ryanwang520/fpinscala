package recfun

import common._

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
   * p(2,3) = p(1,2) + p(2,2)
   * pascal(col, row) =  pascal(col-1,row-1)+pascal(col,row-1)
   * if col == 0 -> 1
   * if row == 0 -> 1
   * if row == col -> 1
   */
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case `r` => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    /**
     *
     * @param part: List[Char] a List of Char
     * @param num: Int current continuous '(' number
     * @return Boolean
     */
    def balanceWithLeftNum(part: List[Char], num: Int): Boolean = {
      assert(num >= 0)
      part match {
        case Nil =>  num == 0
        case head :: tail =>
          head match {
            case '(' => balanceWithLeftNum(tail, num + 1)
            case ')' => if (num == 0) false else balanceWithLeftNum(tail, num - 1)
            case _ => balanceWithLeftNum(tail, num)
          }
      }
    }
    balanceWithLeftNum(chars, 0)
  }

  /**
   * Exercise 3
   * 2种情况
   * 1 countChange(money-head,coins) 使用了head
   * 2 countChange(money,tail)不适用head
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    coins match {
      case Nil => 0
      case head :: tail =>
        if (head > money) countChange(money, tail)
        else if ((money - head) == 0) 1 + countChange(money, tail)
        else countChange(money - head, coins) + countChange(money, tail)
    }
  }
}
