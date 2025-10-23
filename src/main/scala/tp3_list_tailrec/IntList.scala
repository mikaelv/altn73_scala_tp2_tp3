package tp3_list_tailrec

import scala.annotation.tailrec

object IntList:
  def empty: IntList = Empty

case object Empty extends IntList

case class Cons(head: Int, tail: IntList) extends IntList

sealed trait IntList:
  def prepend(element: Int): IntList = Cons(element, this)

  def length: Int =
    @tailrec
    def loop(acc: Int, element: IntList): Int = element match
      case Empty => acc
      case Cons(head, tail) => loop(acc + 1, tail)
    loop(0, this)

  @tailrec
  final def contains(element: Int): Boolean = this match
    case Empty => false
    case Cons(head, tail) =>
      if element == head then true
      else tail.contains(element)

  final def filter(predicate: Int => Boolean): IntList =
    @tailrec
    def loop(acc: IntList, element: IntList): IntList = element match
      case Empty => acc
      case Cons(head, tail) =>
        if predicate(head) then
          loop(Cons(head, acc), tail) // inverse la liste
        else
          loop(acc, tail)

    loop(Empty, this)


  def mkString: String =
    @tailrec
    def loop(acc: String, element: IntList): String = element match
      case Empty => acc
      case Cons(head, tail) =>
        if acc == "" then
          loop(head.toString, tail)
        else
          loop(acc + ", " + head, tail)

    loop("", this)





