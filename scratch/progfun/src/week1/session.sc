object session {
	1+3                                       //> res0: Int(4) = 4
	def abs( x: Double ) = if ( x < 0 ) -x else x
                                                  //> abs: (x: Double)Double
	
	def isGoodEnough( g: Double, x: Double ) : Boolean =
	   abs( g * g  - x ) / x  < 0.001         //> isGoodEnough: (g: Double, x: Double)Boolean
	
		// make test proportional
		// distance between one large number to next can be larger than the threshold
	   // abs bad for small numbers
	   //abs( g * g  - x ) < 0.001
	   
	
	def improve( g: Double, x: Double) =
	( g + x / g ) /2                          //> improve: (g: Double, x: Double)Double
	
	def sqrtIter( g: Double, x: Double ) : Double =
   if ( isGoodEnough( g, x )) g
   else sqrtIter( improve( g, x ), x )            //> sqrtIter: (g: Double, x: Double)Double
   
 	def sqrt( x: Double ) = sqrtIter( 1.0, x )//> sqrt: (x: Double)Double
 	
 	sqrt(2)                                   //> res1: Double = 1.4142156862745097
 	sqrt(4)                                   //> res2: Double = 2.000609756097561
 
  sqrt(0.001)                                     //> res3: Double = 0.03162278245070105
  sqrt(0.1e-20)                                   //> res4: Double = 3.1633394544890125E-11
 	sqrt(1.0e20)                              //> res5: Double = 1.0000021484861237E10
 	sqrt(1.0e50)                              //> res6: Double = 1.0000003807575104E25
 	1.0e20                                    //> res7: Double(1.0E20) = 1.0E20
  "%f".format(1.0e20)                             //> res8: String = 100000000000000000000.000000
 	Math.sqrt( 0.001 )                        //> res9: Double = 0.03162277660168379
 	Math.sqrt( 0.1e-20)                       //> res10: Double = 3.1622776601683794E-11
 
 	sqrt(1e-6)                                //> res11: Double = 0.0010000001533016628
 	Math.sqrt(1e-6)                           //> res12: Double = 0.0010
 
 	
}