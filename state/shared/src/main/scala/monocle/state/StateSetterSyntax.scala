package monocle.state

import monocle.PSetter

import scalaz.{IndexedState, State}

trait StateSetterSyntax {
  implicit def toStateSetterOps[S, T, A, B](setter: PSetter[S, T, A, B]): StateSetterOps[S, T, A, B] =
    new StateSetterOps[S, T, A, B](setter)
}

final class StateSetterOps[S, T, A, B](setter: PSetter[S, T, A, B]) {
  /** modify the value referenced through the setter */
  def modi(f: A => B): IndexedState[S, T, Unit] =
    IndexedState(s => (setter.modify(f)(s), ()))

  /** set the value referenced through the setter */
  def assigni(b: B): IndexedState[S, T, Unit] =
    modi(_ => b)
}
