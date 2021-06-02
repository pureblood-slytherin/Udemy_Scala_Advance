package Lectures.Part1

object AdvancePatternMatching extends App{

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"$head is only number")
    case _ => println("Nothing")
  }
  description

  class Person(val name:String, val age:Int)
  // if we cannot make case class, then we can add unapply method in the companion object that give us the output we want to match
  object Person{
    def unapply(arg: Person):Option[(String,Int)]= Some((arg.name,arg.age))
    // We can overload the unapply Method
    def unapply(age:Int):Option[String]=
      Some(if (age<21) "Minor" else "Major")
  }
  val Harry = new Person("Harry",11)
  val greet= Harry match {
    case Person(a,b) => println(s"I am $a and I am $b years old")
  }
  greet
  val legalStatus = Harry.age match {
    case Person(status) => println(s"Harry's legal status was $status")
  }
  legalStatus

  // Exercise

  val myInt = 45
  val property = myInt match {
    case x if x > -10 && x< 10 => "Single Digit"
    case x if x%2 ==0 => "Even number"
    case _ => "No property matched"
  }
  println(property)

  object even{
    def unapply(arg :Int):Boolean= arg%2==0
  }
  object singleDigit{
    def unapply(arg:Int):Boolean= arg > -10 && arg < 10
  }
  val fancyProperty = myInt match {
    case singleDigit() => "Single digit"
    case even() => "Even"
    case _ => "No property matched"
  }
  println(fancyProperty)

  //Infix pattern
  case class Or(numb:Int,number:String)
  val either = Or(2,"two")
  val patternMatch = either match {
    case number Or string => s"$number is written as $string"
  }
  println(patternMatch)
  val myList1 = List(2,3,4,5,6)
  val decomposingList = myList1 match {
    case List(2,_*) => "Starts from two"
  }
  println(decomposingList)

  // decomposing custom sequence
  abstract class myList[+A]{
    def head:A = ???
    def tail: myList[A] = ???
  }
  case object Empty extends myList[Nothing]
  case class Cons[+A](override val head: A ,override val tail: myList[A]) extends myList[A]

  object myList{
    def unapplySeq[A](list : myList[A]):Option[Seq[A]]=
      if(list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }
  val myCreatedList = Cons(2,Cons(3,Cons(5,Empty)))
  val decmposedList = myCreatedList match {
    case myList(2,3,_*) => "Started by 2,3"
    case _=> "Nothing"
  }
  println(decmposedList)

  // Unapply can return custom types also
  abstract class Wrapper[T]{
    def isEmpty: Boolean
    def get:T
  }
  object PersonWrapper{
    def unapply(person: Person):Wrapper[String]= new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }
  val HarryMatch = Harry match {
    case PersonWrapper(n) => s"His name is $n"
    case _=> "No match"
  }

  println(HarryMatch)
}
