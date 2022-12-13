import scala.annotation.tailrec

case class Monkey(
                 monkeyId: Int,
                 operation: Long => Long,
                 divisor: Int,
                 trueMonkey: Int,
                 falseMonkey: Int,
                 itemsOwned: Seq[Long],
                 totalInteractions: Long = 0
                 ) {
  private def interact(superMod: Long): Seq[Long] = itemsOwned.map(operation(_) % superMod)
  def chuck(superMod: Long): (Seq[Long], Seq[Long]) = interact(superMod).partition(_%divisor == 0)
}


object DayEleven extends App {
  val testMonkeys = Seq(
    Monkey(0, _*19, 23, 2,3 , Seq(79, 98)),
    Monkey(1, _+6, 19, 2, 0, Seq(54, 65, 75, 74)),
    Monkey(2, (wor: Long) => wor*wor, 13, 1, 3, Seq(79, 60, 97)),
    Monkey(3, _+3, 17, 0, 1, Seq(74))
  )

  val monkeys = Seq(
    Monkey(0, _*5, 3, 7, 4, Seq(66, 71, 94)),
    Monkey(1, _+6, 17, 3, 0, Seq(70)),
    Monkey(2, _+5, 2, 3, 1, Seq(62, 68, 56, 65, 94, 78)),
    Monkey(3, _+2, 19, 7, 0, Seq(89, 94, 94, 67)),
    Monkey(4, _*7, 11, 5, 6, Seq(71, 61, 73, 65, 98, 98, 63)),
    Monkey(5, _+7, 5, 2, 1, Seq(55, 62, 68, 61, 60)),
    Monkey(6, _+1, 13, 5, 2, Seq(93, 91, 69, 64, 72, 89, 50, 71)),
    Monkey(7, (wor: Long) => wor*wor, 7, 4, 6, Seq(76, 50))
  )

  val superMod: Long = monkeys.foldLeft(1)(_ * _.divisor)

  @tailrec
  def iterateMonkey(monkeys: Seq[Monkey], monkeyId: Int = 0): Seq[Monkey] = {
    if (monkeyId == monkeys.size) monkeys
    else {
      val monkeyInQuestion = monkeys(monkeyId)
      val (trueWorries, falseWorries) = monkeyInQuestion.chuck(superMod)
      val trueMonkey = monkeyInQuestion.trueMonkey
      val falseMonkey = monkeyInQuestion.falseMonkey
      val interactionCount = monkeyInQuestion.itemsOwned.size
      iterateMonkey(monkeys.map{ monkey =>
        if (monkey.monkeyId == trueMonkey) monkey.copy(itemsOwned = monkey.itemsOwned ++ trueWorries)
        else if (monkey.monkeyId == falseMonkey) monkey.copy(itemsOwned = monkey.itemsOwned ++ falseWorries)
        else if (monkey.monkeyId == monkeyId) monkey.copy(itemsOwned = Seq(), totalInteractions = monkey.totalInteractions + interactionCount)
        else monkey
      }, monkeyId + 1)
    }
  }

  @tailrec
  def iterateRound(monkeys: Seq[Monkey], iteration: Int = 0): Seq[Monkey] = {
    if (iteration == 10000) monkeys
    else iterateRound(iterateMonkey(monkeys), iteration + 1)
  }

  val tiredMonkeys = iterateRound(monkeys).map(_.totalInteractions).sorted.reverse

  println(tiredMonkeys(0)*tiredMonkeys(1))
}
