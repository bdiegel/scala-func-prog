object session02 {

	def abs( x: Double ) = if ( x < 0 ) -x else x
                                                  //> abs: (x: Double)Double
  def sqrt(x: Double) = {

    def sqrtIter(g: Double ): Double =
      if (isGoodEnough(g)) g
      else sqrtIter(improve(g))

    def isGoodEnough(g: Double ): Boolean =
      abs(g * g - x) / x < 0.001

    def improve(g: Double) =
      (g + x / g) / 2


    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
 		
}