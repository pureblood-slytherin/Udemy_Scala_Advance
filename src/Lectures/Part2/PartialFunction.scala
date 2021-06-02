package Lectures.Part2

object PartialFunction extends App{
  // A function that takes only 3 ints, 1,2 5
  val OldPartialFunction= (x:Int)=> {
    if(x==1) 42
    else if(x==2) 34
    else if(x== 5) 86
    else throw new PartialFunctionError
  }
  class PartialFunctionError extends RuntimeException
  //OldPartialFunction(4)
  val betterPartailFunction = (x:Int)=> x match {
    case 2 => 42
    case 1=> 34
    case 5=> 89
  }
  println(betterPartailFunction(2))

  val actualPartialFUnction : PartialFunction[Int,Int]={
    case 2 => 42
    case 1=> 34
    case 5=> 89
  }
  println(actualPartialFUnction(5))
  // Partial Function Utilities
  println(actualPartialFUnction.isDefinedAt(45))
  println(actualPartialFUnction.lift(9))
  println(actualPartialFUnction.lift(2))

  // Extending Partial function
  val newPartialFunction = actualPartialFUnction.orElse[Int,Int]{
    case 7 =>78
  }
  println(newPartialFunction(2))
  println(newPartialFunction(7))

  // HOFs can also take Partial function

  val myMappedList = List(1,2,3).map{
    case 1 => 43
    case 2 => 34
    case 3 => 67
  }
  println(myMappedList)
  // PARTIAL FUNCTION HAVE ONLY ONE PARAMETER TYPE
  // Exercise:
  // 1. Construct a PF instance of yourSelf(anonymous class)
  // 2. dumb ChatBot as a PF
  val mySquare = new PartialFunction[Int,Int]{
    override def apply(v1: Int): Int = v1 match {
      case 1 => 1
      case 2 =>4
      case 3 => 6
    }

    override def isDefinedAt(x: Int): Boolean =
      x ==1| x== 2| x==3
  }
  val chatBot: PartialFunction[String,String]= {
    case "Hi" => "Hi, how are you ?"
    case "I am Harry" => "Hi Harry! I am Tom Riddle"
  }

  //scala.io.Source.stdin.getLines().foreach(line => println("Reply: " + chatBot(line)))
  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)

}
