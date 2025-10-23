package tp3_list

sealed trait IntList:
  def prepend(element: Int): IntList = 
    Cons(element, this)

  def length: Int

  def contains(element: Int): Boolean

  def filter(predicate: Int => Boolean): IntList

  def mkString: String

  def double: IntList

  def map(transfo: Int => Int): IntList

  def sum: Int


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

  override def double: IntList =
    this

  override def map(transfo: Int => Int): IntList =
    this

  override def sum: Int = 
    0
  
    
case class Cons(head: Int, tail: IntList) extends IntList:
  override def length: Int = 
    1 + tail.length

  override def contains(element: Int): Boolean =
    if element == head then true
    else tail.contains(element)

  override def filter(predicate: Int => Boolean): IntList = 
    if predicate(head) then Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  override def mkString: String = 
    head + ", " + tail.mkString    
    
  override def double: IntList =
    Cons(head * 2, tail.double)

  override def map(transfo: Int => Int): IntList =
    Cons(transfo(head), tail.map(transfo))

  override def sum: Int = 
    head + tail.sum
  

