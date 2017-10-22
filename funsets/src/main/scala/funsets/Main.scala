package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)
  println(contains(s1, 1))
  printSet(map(s1, x => x * 3 ))
  printSet(map(s1, x => x + 2 ))
}
