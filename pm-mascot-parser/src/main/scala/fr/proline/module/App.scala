package fr.proline.module

/**
 * @author ${user.name}
 */
object App {
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
    println( "Hello World!" )
    println("concat arguments = " + foo(args))
    
//    val nbrQueryies:Int = 15
//    val maxRankPerQuery:Int  = 10
//    
//    for(i <- 1 until nbrQueryies){
//      println( " DEF Q "+i )
//      for(k <- 1 until maxRankPerQuery )
//        println( "DEF Pep "+ i+"-"+k )
//      
//    }
    
       
    var str = "Oxidation (M)"
     str.split(",") foreach { (x) =>
       println( "next splitted "+ x)
     }
//    var residues = "-"
//    if(str.contains("(")){
//    	residues = str.substring(str.indexOf("("))
//        str = str.substring(0,str.indexOf("(")-1)
//    }
//     println( "residues "+ residues+" str "+str )
//     
//      var residuChars:Array[Char] = "".toCharArray()   
//      for (k <- 0 until residuChars.length) {
//         println( "next residu "+ residuChars(k) )
//      }
  }
  

}
