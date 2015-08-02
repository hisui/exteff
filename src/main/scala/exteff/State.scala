package exteff

import scala.reflect.runtime.universe.TypeTag

import Union._
import Eff.Eff1

sealed trait IntState[A]
private case class Get[A](k: Int  => A) extends IntState[A]
private case class Set[A](k: Unit => A, n: Int)
  extends IntState[A]

object IntState {

  implicit def functor = new Functor[IntState] {
    def fmap[A, B](f: A => B)(ma: IntState[A]): IntState[B] = ma match {
      case Get(k   ) => Get[B](n => f(k(n)))
      case Set(k, n) => Set[B](_ => f(k(())), n)
    }
  }

  def get: Eff1[IntState, Int] = Eff.send(new New[Int, IntState |: `[]`]
  {
    def apply[W: TypeTag, R2 <: HList : TypeTag]
    (k: Int => VE[W, R2])(implicit ev: Subset[IntState |: `[]`, R2]): Union[R2, VE[W, R2]] =
      Union(Get(k) : IntState[VE[W, R2]])
  })

  def set(n: Int): Eff1[IntState, Unit] = Eff.send(new New[Unit, IntState |: `[]`]
  {
    def apply[W: TypeTag, R2 <: HList : TypeTag]
    (k: Unit => VE[W, R2])(implicit ev: Subset[IntState |: `[]`, R2]): Union[R2, VE[W, R2]] =
      Union(Set(k, n) : IntState[VE[W, R2]])
  })

  def run[A: TypeTag
    , R <: HList : TypeTag
    , S <: HList : TypeTag](ex: Eff[R, A], n: Int)
  (implicit ev1: Remove[IntState, R, S]): Eff[S, A] =
  {
    def loop(n: Int)(ev: VE[A, R]): Eff[S, A] = {
      def handle(r: IntState[VE[A, R]]): Eff[S, A] = r match {
        case Get(k   ) => loop(n)(k( n))
        case Set(k, n) => loop(n)(k(()))
      }
      ev match {
        case x: Val[A, _] => Eff(x.value)
        case E(u) =>
          Eff.handleRelay(u)(loop(n), handle)
      }
    }
    loop(n)(Eff.admin(ex))
  }

}
