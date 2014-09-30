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
   */
  def pascal(c: Int, r: Int): Int =
    if (c > r || c < 0) throw new NoSuchElementException("Coordinate is outside of triangle")
    else if (r == 0 || c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

      def balanceIter(subChars: List[Char], bracketCount: Int): Boolean = {
        if (bracketCount < 0) false
        else if (subChars.isEmpty) (bracketCount == 0)
        else if (subChars.head == ')') balanceIter(subChars.tail, bracketCount-1)
        else if (subChars.head == '(') balanceIter(subChars.tail, bracketCount+1)
        else balanceIter(subChars.tail, bracketCount)
      }
      
      balanceIter(chars, 0)
  } 

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def sortedCountChange(remainingMoney: Int, remainingCoins: List[Int]) : Int = {
      if (remainingCoins.isEmpty || remainingMoney < 0) 0
      else if (remainingMoney == 0) 1
      else sortedCountChange(remainingMoney - remainingCoins.head, remainingCoins) + 
           sortedCountChange(remainingMoney, remainingCoins.tail)
    }
    
    if (coins.isEmpty) 0
    else if (money == 0) 0
    else sortedCountChange(money, coins)
  }
}
