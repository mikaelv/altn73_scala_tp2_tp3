package tp3_list_tailrec

object Main:
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
    println(s"list3.filter(_ % 2 == 0): " + list3.filter(x => x % 2 == 0))

    println("\nTransformation et filtrage")
    println(s"list3.double: ${list3.double}")
    println(s"list3.map(_ + 1): ${list3.map(_ + 1)}")
    println(s"list3.sum: ${list3.sum}")

