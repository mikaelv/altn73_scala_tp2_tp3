package tp3_list

object Main {
  @main def testIntList1(): Unit =
    println("Premières méthodes")
    val empty = IntList.empty
    val list1 = empty.prepend(1)
    val list2 = list1.prepend(2)
    println(s"list2: $list2")
    println(s"list2.length : ${list2.length}")

    println("\nMéthodes plus avancées")
    val list3 = IntList.empty.prepend(4).prepend(3).prepend(2).prepend(1)
    println(s"list3: $list3")
    println(s"list3.mkString: ${list3.mkString}")
    println("filtered: " + list3.filter(x => x % 2 == 0))
}
