package Exercise

import scala.annotation.tailrec

trait MySet[A] extends (A=>Boolean) {
  def apply(v1: A): Boolean =
    contains(v1)
  def contains(element:A):Boolean
  def +(element:A):MySet[A]
  def ++(newSet:MySet[A]):MySet[A]

  def map[B](f:A=>B):MySet[B]
  def flatMap[B](f: A=> MySet[B]):MySet[B]
  def filter(f:A=>Boolean):MySet[A]
  def ofEach(f:A=>Unit):Unit

}
class EmptySet[A] extends MySet[A]{
  def contains(element: A): Boolean = false
  def +(element: A): MySet[A] = new NonEmptySet[A](element,this)
  def ++(newSet:MySet[A]):MySet[A] = newSet

  def map[B](f:A=>B):MySet[B] = new EmptySet[B]
  def flatMap[B](f: A=> MySet[B]):MySet[B] = new EmptySet[B]
  def filter(f:A=>Boolean):MySet[A] = this
  def ofEach(f:A=>Unit):Unit  = ()
}

class NonEmptySet[A](head:A,tail:MySet[A]) extends MySet[A]{
  def contains(element:A):Boolean =
    head == element || tail.contains(element)
  def +(element:A):MySet[A] =
    if(this.contains(element)) this
    else new NonEmptySet[A](element,this)

  def ++(newSet:MySet[A]):MySet[A]=
    tail ++ newSet + head

  def map[B](f:A=>B):MySet[B] =
    new NonEmptySet[B](f(head),tail.map(f))
  // (tail map f) + f(head)
  def flatMap[B](f: A=> MySet[B]):MySet[B] =
    (tail flatMap f) ++ f(head)
  def filter(f:A=>Boolean):MySet[A] = {
    val filteredTail = tail filter f
    if(f(head)) filteredTail+head
    else filteredTail
  }

  def ofEach(f:A=>Unit):Unit ={
    f(head)
    tail ofEach f
  }

}

object MySet{
  def apply[A](values: A*):MySet[A] ={
    @tailrec
    def builtSet(valSeq: Seq[A],acc: MySet[A]):MySet[A]=
      if(valSeq.isEmpty) acc
      else builtSet(valSeq.tail,acc+ valSeq.head)

    builtSet(values,new EmptySet[A])
  }
}

object mainFunction extends App{
  val s = MySet(1,2,3,4)
  s + 5 ofEach println
  val otherSet = MySet(-9,3,-1)
  (s ++ otherSet) ofEach println
  val newSet = s ++ otherSet
  (newSet.map(_*10)) ofEach println


}
