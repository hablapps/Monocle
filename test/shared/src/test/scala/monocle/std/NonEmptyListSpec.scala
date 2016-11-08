package monocle.std

import monocle.MonocleSuite
import monocle.law.discipline.IsoTests
import monocle.law.discipline.function._

import scalaz.{IList, NonEmptyList}

class NonEmptyListSpec extends MonocleSuite {
  checkAll("nelToAndOne", IsoTests(nelToOneAnd[Int]))
  checkAll("optNelToList", IsoTests(optNelToList[Int]))

  checkAll("each NonEmptyList", EachTests[NonEmptyList[Int], Int])
  checkAll("index NonEmptyList", IndexTests[NonEmptyList[Int], Int, Int])
  checkAll("filterIndex NonEmptyList", FilterIndexTests[NonEmptyList[Int], Int, Int])
  checkAll("reverse NonEmptyList", ReverseTests[NonEmptyList[Int]])
  checkAll("cons1 NonEmptyList", Cons1Tests[NonEmptyList[Int], Int, IList[Int]])
  checkAll("snoc1 NonEmptyList", Snoc1Tests[NonEmptyList[Int], IList[Int], Int])
}
