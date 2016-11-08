package monocle.function

import monocle.MonocleSuite
import monocle.law.discipline.function.AtTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class AtSpec extends MonocleSuite with GeneratorDrivenPropertyChecks {

  implicit def mmapAt[K, V]: At[MMap[K, V], K, Option[V]] = At.fromIso(MMap.toMap)

  checkAll("fromIso", AtTests[MMap[Int, String], Int, Option[String]])

  test("remove deletes a key") {

    val mapAndIndexGen: Gen[(Map[Int, String], Int)] = for {
      m <- Arbitrary.arbitrary[Map[Int, String]]
      i <- if(m.isEmpty) Arbitrary.arbInt.arbitrary
      else Gen.frequency(
        (8, Gen.oneOf(m.keys.toList)),
        (2, Arbitrary.arbInt.arbitrary))
    } yield (m, i)

    forAll(mapAndIndexGen) { case (m, i) =>
      remove(i)(m) should be (m - i)
    }
  }

}
