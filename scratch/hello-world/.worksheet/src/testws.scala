object testws {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(59); 
  println("Welcome to the Scala worksheet");$skip(15); 
  
  val x = 1;System.out.println("""x  : Int = """ + $show(x ));$skip(34); 
  def increase( i : Int ) = i + 1;System.out.println("""increase: (i: Int)Int""");$skip(16); val res$0 = 
  increase( x );System.out.println("""res0: Int = """ + $show(res$0))}
}