package tp3_list

import scala.annotation.tailrec

object IntList:
  def empty: IntList = Empty

case object Empty extends IntList:
  override def length: Int = 
    0

  override def contains(element: Int): Boolean = 
    false

  override def filter(predicate: Int => Boolean): IntList =
    this

  override def mkString: String = 
    ""

case class Cons(head: Int, tail: IntList) extends IntList:
  override def length: Int = 
    1 + tail.length

  override def contains(element: Int): Boolean =
    if element == head then true
    else tail.contains(element)

  def filter(predicate: Int => Boolean): IntList = 
    if predicate(head) then Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  def mkString: String = 
    head + ", " + tail.mkString    
  
  

sealed trait IntList:
  def prepend(element: Int): IntList = Cons(element, this)

  def length: Int

  def contains(element: Int): Boolean

  def filter(predicate: Int => Boolean): IntList 

  def mkString: String


