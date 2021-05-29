package Lectures.Part1

object DarkSugars extends App{
  // syntax sugar 1 : Methods with single parameter
  def singleArgFunc( input: Int)= input+1
  val testSingleArgFunc = singleArgFunc{
    val complex_Calculation = 45
    complex_Calculation+1
  }
  // Try function also uses this
  List(1,2,3,4).map{ x=>
    x+1
  }

  //Syntax sugar :2 Single Abstract method
  trait singleAddition {
    def add(a:Int, b:Int):Int
  }
  // instance of anonymous class
  val myAdd:singleAddition = new singleAddition {
    override def add(a: Int, b: Int): Int = a+b
  }
  val myMagicAdd:singleAddition = (x:Int,y:Int)=> x+y  // lambda function
  // Also with if other methods are implemented and only one method in not implemented in a abstract class
  abstract class myAbstractClass {
    def implemented:String= "I am implemented"
    def unImplemented(a:Int):Int
  }
  val mYInstanceAbstract: myAbstractClass = (x:Int)=>x*x

  //Syntax Sugar 3:   :: & ::# methods
  val prependList = 2 :: List(3,4)
  // Ideally it should mean 2.::(List(3,4))
  // But it actally means List(3,4).::(2)

  // Scala SPec: last character decides the associativity of method
  // ":" means right associative

  1:: 2:: List(3,4)
  List(3,4).::(2).::(1) // this is how th above line is complied

  class myStream[T]{
    def -->: (value:T):myStream[T] =this
  }

  val mystream = 1 -->: 2-->: 3-->: new myStream[Int]

  // Sysntax sugar 4: Multi word name
  class name(name:String){
    def `have always loved`(n:String):String = s"$name have always loved $n"
  }
  val Ross = new name("Ross")
  println(Ross `have always loved` "Rachel")

  // Syntax sugar 5 : Infix type
  class comp[A,B]
  val myComp: Int comp String = new comp

  class -->[A,B]
  //val towards: Int --> String = ???

  // Mutable Containers

  // Syntax Sugar 6: Update Method
  val myArray = Array(1,2,3)
  myArray(2) = 8
  println(myArray.mkString(" "))
  myArray.update(2,1)
  println(myArray.mkString(" "))

  // Syntax sugar 7: Setter
  class Mutable{
    private var internalMember =0
    def member = internalMember
    def member_=(value:Int) =
      internalMember=value
  }
  val aMutableObject = new Mutable
  aMutableObject.member = 7 // aMutableObject.member_=(7)




}
