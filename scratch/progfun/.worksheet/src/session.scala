object session {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(21); val res$0 = 
	1+3;System.out.println("""res0: Int(4) = """ + $show(res$0));$skip(47); 
	def abs( x: Double ) = if ( x < 0 ) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(91); 
	
	def isGoodEnough( g: Double, x: Double ) : Boolean =
	   abs( g * g  - x ) / x  < 0.001;System.out.println("""isGoodEnough: (g: Double, x: Double)Boolean""");$skip(238); 
	
		// make test proportional
		// distance between one large number to next can be larger than the threshold
	   // abs bad for small numbers
	   //abs( g * g  - x ) < 0.001
	   
	
	def improve( g: Double, x: Double) =
	( g + x / g ) /2;System.out.println("""improve: (g: Double, x: Double)Double""");$skip(122); 
	
	def sqrtIter( g: Double, x: Double ) : Double =
   if ( isGoodEnough( g, x )) g
   else sqrtIter( improve( g, x ), x );System.out.println("""sqrtIter: (g: Double, x: Double)Double""");$skip(49); 
   
 	def sqrt( x: Double ) = sqrtIter( 1.0, x );System.out.println("""sqrt: (x: Double)Double""");$skip(13); val res$1 = 
 	
 	sqrt(2);System.out.println("""res1: Double = """ + $show(res$1));$skip(10); val res$2 = 
 	sqrt(4);System.out.println("""res2: Double = """ + $show(res$2));$skip(16); val res$3 = 
 
  sqrt(0.001);System.out.println("""res3: Double = """ + $show(res$3));$skip(16); val res$4 = 
  sqrt(0.1e-20);System.out.println("""res4: Double = """ + $show(res$4));$skip(15); val res$5 = 
 	sqrt(1.0e20);System.out.println("""res5: Double = """ + $show(res$5));$skip(15); val res$6 = 
 	sqrt(1.0e50);System.out.println("""res6: Double = """ + $show(res$6));$skip(9); val res$7 = 
 	1.0e20;System.out.println("""res7: Double(1.0E20) = """ + $show(res$7));$skip(22); val res$8 = 
  "%f".format(1.0e20);System.out.println("""res8: String = """ + $show(res$8));$skip(21); val res$9 = 
 	Math.sqrt( 0.001 );System.out.println("""res9: Double = """ + $show(res$9));$skip(22); val res$10 = 
 	Math.sqrt( 0.1e-20);System.out.println("""res10: Double = """ + $show(res$10));$skip(15); val res$11 = 
 
 	sqrt(1e-6);System.out.println("""res11: Double = """ + $show(res$11));$skip(18); val res$12 = 
 	Math.sqrt(1e-6);System.out.println("""res12: Double = """ + $show(res$12))}
 
 	
}