package monocle.state

import monocle.POptional

import scalaz.{IndexedState, State}

trait StateOptionalSyntax {
  implicit def toStateOptionalOps[S, T, A, B](optional: POptional[S, T, A, B]): StateOptionalOps[S, T, A, B] =
    new StateOptionalOps[S, T, A, B](optional)
}

final class StateOptionalOps[S, T, A, B](optional: POptional[S, T, A, B]) {
  /** transforms a POptional into a State */
  def toState: State[S, Option[A]] =
    State(s => (s, optional.getOption(s)))

  /** alias for toState */
  def st: State[S, Option[A]] =
    toState

    /** extracts the value viewed through the optional */
  def extract: State[S, Option[A]] =
    toState

  /** extracts the value viewed through the optional and applies `f` over it */
  def extracts[B](f: A => B): State[S, Option[B]] =
    extract.map(_.map(f))

  /** modify the value viewed through the Optional and return its *new* value, if there is one */
  def mod(f: A => B): IndexedState[S, T, Option[B]] =
    IndexedState(s => (optional.modify(f)(s), optional.getOption(s).map(f)))

  /** modify the value viewed through the Optional and return its *old* value, if there was one */
  def modo(f: A => B): IndexedState[S, T, Option[A]] =
    IndexedState(s => (optional.modify(f)(s), optional.getOption(s)))

  /** modify the value viewed through the Optional and ignores both values */
  def modi(f: A => B): IndexedState[S, T, Unit] =
    IndexedState(s => (optional.modify(f)(s), ()))

  /** set the value viewed through the Optional and returns its *new* value */
  def assign(b: B): IndexedState[S, T, Option[B]] =
    IndexedState(s => (optional.set(b)(s), Option(b)))

  /** set the value viewed through the Optional and return its *old* value, if there was one */
  def assigno(b: B): IndexedState[S, T, Option[A]] =
    IndexedState(s => (optional.set(b)(s), optional.getOption(s)))

  /** set the value viewed through the Optional and ignores both values */
  def assigni(b: B): IndexedState[S, T, Unit] =
    IndexedState(s => (optional.set(b)(s), ()))
}
