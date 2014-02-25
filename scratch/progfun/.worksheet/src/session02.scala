object session02 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(66); 

	def abs( x: Double ) = if ( x < 0 ) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(281); 
  def sqrt(x: Double) = {

    def sqrtIter(g: Double ): Double =
      if (isGoodEnough(g)) g
      else sqrtIter(improve(g))

    def isGoodEnough(g: Double ): Boolean =
      abs(g * g - x) / x < 0.001

    def improve(g: Double) =
      (g + x / g) / 2


    sqrtIter(1.0)
  };System.out.println("""sqrt: (x: Double)Double""")}
 		
}