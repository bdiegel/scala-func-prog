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
    if ( c==0 || c==r ) 1
    else pascal( c-1, r-1 ) + pascal( c, r-1 )

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceIter( count: Int, cs: List[Char] ) : Boolean =
    	if ( cs.isEmpty ) count==0
    	else
    		if ( count<0 ) false
    		else
    			if ( cs.head == '(' ) balanceIter( count+1, cs.tail )
    			else 
    				if ( cs.head == ')' ) balanceIter( count-1, cs.tail )
    				else balanceIter( count, cs.tail )
    				
     balanceIter( 0, chars )
   }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if ( money==0 ) 1
    else 
    	if ( money<0 || coins.isEmpty ) 0
    	else ( countChange(money, coins.tail ) + countChange( money-coins.head, coins ) )
    	
  def countChange2(money: Int, coins: List[Int]): Int = 
    ( money, coins ) match {
    	  case ( 0, _ )	     => 1
    	  case ( _, List() ) => 0
    	  case ( m, h::t )   => countChange( m, t ) + countChange( m-h, coins ) 
    }
    	

    	
  def balance2(chars: List[Char]): Boolean = {

    def balanceIter( count: Int, cs: List[Char] ) : Boolean =
      (count, cs) match {
      	case ( c, List()  )	          => c==0
      	case ( c, _       ) if c<0	  => false
      	case ( c, h::tail ) if h=='(' => balanceIter( c+1, tail )
      	case ( c, h::tail ) if h==')' => balanceIter( c-1, tail )
      	case ( c, _::tail )           => balanceIter( c, tail )
      }
      
//    	if ( cs.isEmpty ) count==0
//    	else
//    		if ( count<0 ) false
//    		else
//    			if ( cs.head == '(' ) balanceIter( count+1, cs.tail )
//    			else 
//    				if ( cs.head == ')' ) balanceIter( count-1, cs.tail )
//    				else balanceIter( count, cs.tail )
    				
     balanceIter( 0, chars )
   }    	
}
