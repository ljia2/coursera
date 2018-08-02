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
      if(c == 0 || c == r) return 1
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def checkBalance(leftParentThesis: Int, chars: List[Char]): Boolean = {
        if(leftParentThesis < 0) return false
        if(chars.isEmpty) return leftParentThesis == 0
        chars.head match {
          case '(' => checkBalance(leftParentThesis + 1, chars.tail)
          case ')' => checkBalance(leftParentThesis -1, chars.tail)
          case _ => checkBalance(leftParentThesis, chars.tail)
        }
      }

      if(chars.isEmpty) return true
      chars.head match {
        case '(' => checkBalance(1, chars.tail)
        case ')' => false
        case _ => checkBalance(0, chars.tail)
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money <= 0 || coins.isEmpty) return 0
      val sortedCoins = coins.sortWith(_<_)
      if(money < sortedCoins.head) return 0
      if(money == sortedCoins.head) return 1
      countChange(money - sortedCoins.head, sortedCoins) + countChange(money, sortedCoins.tail)
    }
  }
