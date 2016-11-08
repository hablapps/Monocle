package monocle.function

import monocle.{Iso, PTraversal, Traversal}

import scala.annotation.implicitNotFound
import scalaz.{Applicative, Traverse}

/**
 * Typeclass that defines a [[Traversal]] from a monomorphic container `S` to all of its elements of type `A`
 * @tparam S source of [[Traversal]]
 * @tparam A target of [[Traversal]], `A` is supposed to be unique for a given `S`
 */
@implicitNotFound("Could not find an instance of Each[${S},${A}], please check Monocle instance location policy to " +
  "find out which import is necessary")
abstract class Each[S, A] extends Serializable {
  def each: Traversal[S, A]
}


trait EachFunctions {
  def each[S, A](implicit ev: Each[S, A]): Traversal[S, A] = ev.each

  def traverseEach[S[_]: Traverse, A]: Each[S[A], A] = new Each[S[A], A] {
    def each = PTraversal.fromTraverse[S, A, A]
  }
}

object Each extends EachFunctions {
  /** lift an instance of [[Each]] using an [[Iso]] */
  def fromIso[S, A, B](iso: Iso[S, A])(implicit ev: Each[A, B]): Each[S, B] = new Each[S, B] {
    val each: Traversal[S, B] =
      iso composeTraversal ev.each
  }

  /************************************************************************************************/
  /** Std instances                                                                               */
  /************************************************************************************************/
  import scalaz.std.list._
  import scalaz.std.map._
  import scalaz.std.stream._
  import scalaz.std.vector._

  implicit def listEach[A]: Each[List[A], A] = traverseEach

  implicit def mapEach[K, V]: Each[Map[K, V], V] = traverseEach[Map[K, ?], V]

  implicit def optEach[A]: Each[Option[A], A] = new Each[Option[A], A] {
    def each = monocle.std.option.some[A].asTraversal
  }

  implicit def streamEach[A]: Each[Stream[A], A] = traverseEach

  implicit val stringEach: Each[String, Char] = new Each[String, Char] {
    val each = monocle.std.string.stringToList composeTraversal Each.each[List[Char], Char]
  }

  implicit def tuple1Each[A]: Each[Tuple1[A], A] = new Each[Tuple1[A], A] {
    val each = monocle.std.tuple1.tuple1Iso[A].asTraversal
  }

  implicit def tuple2Each[A]: Each[(A, A), A] = new Each[(A, A), A] {
    val each = PTraversal.apply2[(A, A), (A, A), A, A](_._1,_._2)((b1, b2, _) => (b1, b2))
  }

  implicit def tuple3Each[A]: Each[(A, A, A), A] = new Each[(A, A, A), A] {
    val each = PTraversal.apply3[(A, A, A), (A, A, A), A, A](_._1,_._2,_._3)((b1, b2, b3, _) => (b1, b2, b3))
  }

  implicit def tuple4Each[A]: Each[(A, A, A, A), A] = new Each[(A, A, A, A), A] {
    val each = PTraversal.apply4[(A, A, A, A), (A, A, A, A), A, A](_._1,_._2,_._3,_._4)((b1, b2, b3, b4, _) => (b1, b2, b3, b4))
  }

  implicit def tuple5Each[A]: Each[(A, A, A, A, A), A] = new Each[(A, A, A, A, A), A] {
    val each = PTraversal.apply5[(A, A, A, A, A), (A, A, A, A, A), A, A](_._1,_._2,_._3,_._4,_._5)((b1, b2, b3, b4, b5, _) => (b1, b2, b3, b4, b5))
  }

  implicit def tuple6Each[A]: Each[(A, A, A, A, A, A), A] = new Each[(A, A, A, A, A, A), A] {
    val each = PTraversal.apply6[(A, A, A, A, A, A), (A, A, A, A, A, A), A, A](_._1,_._2,_._3,_._4,_._5, _._6)((b1, b2, b3, b4, b5, b6, _) => (b1, b2, b3, b4, b5, b6))
  }

  implicit def vectorEach[A]: Each[Vector[A], A] = traverseEach

  /************************************************************************************************/
  /** Scalaz instances                                                                            */
  /************************************************************************************************/
  import scalaz.{==>>, Cofree, IList, Maybe, NonEmptyList, OneAnd, Tree}

  implicit def cofreeEach[S[_]: Traverse, A]: Each[Cofree[S, A], A] = traverseEach[Cofree[S, ?], A]

  implicit def iListEach[A]: Each[IList[A], A] = traverseEach

  implicit def iMapEach[K, V]: Each[K ==>> V, V] = traverseEach[K ==>> ?, V]

  implicit def maybeEach[A]: Each[Maybe[A], A] = new Each[Maybe[A], A]{
    def each = monocle.std.maybe.just.asTraversal
  }

  implicit def nelEach[A]: Each[NonEmptyList[A], A] = traverseEach

  implicit def oneAndEach[T[_], A](implicit ev: Each[T[A], A]): Each[OneAnd[T, A], A] =
    new Each[OneAnd[T, A], A]{
      val each = new Traversal[OneAnd[T, A], A]{
        def modifyF[F[_]: Applicative](f: A => F[A])(s: OneAnd[T, A]): F[OneAnd[T, A]] =
          Applicative[F].apply2(f(s.head), ev.each.modifyF(f)(s.tail))((head, tail) => new OneAnd(head, tail))
      }
    }

  implicit def treeEach[A]: Each[Tree[A], A] = traverseEach
}