package exteff

import scala.reflect.runtime.universe.TypeTag

import Union._
import Eff.Eff1

sealed trait State[S, A]
private case class Get[S, A](k: S => A) extends State[S, A]
private case class Set[S, A](k: Unit => A, n: S)
  extends State[S, A]

object State {

  implicit def functor[S] = new Functor[({ type z[a] = State[S, a] })#z] {
    def fmap[A, B](f: A => B)(ma: State[S, A]): State[S, B] = ma match {
      case Get(k   ) => Get[S, B](n => f(k(n)))
      case Set(k, n) => Set[S, B](_ => f(k(())), n)
    }
  }

  def get[S: TypeTag] = Eff.send(new New[S, ({ type z[a] = State[S, a] })#z |: `[]`]
  {
    type F[a] = State[S, a]
    def apply[W: TypeTag, R2 <: HList : TypeTag]
    (k: S => VE[W, R2])(implicit ev: ⊆[F |: `[]`, R2]): Union[R2, VE[W, R2]] =
      Union(Get(k) : F[VE[W, R2]])
  })

  def set[S: TypeTag](n: S) = Eff.send(new New[Unit, ({ type z[a] = State[S, a] })#z  |: `[]`]
  {
    type F[a] = State[S, a]
    def apply[W: TypeTag, R2 <: HList : TypeTag]
    (k: Unit => VE[W, R2])(implicit ev: ⊆[F |: `[]`, R2]): Union[R2, VE[W, R2]] =
      Union(Set(k, n) : F[VE[W, R2]])
  })

  def run[A: TypeTag, S: TypeTag
    , R0 <: HList : TypeTag
    , R1 <: HList : TypeTag](ex: Eff[R0, A], n: S)
  (implicit ev1: Remove[({ type z[a] = State[S, a] })#z, R0, R1]): Eff[R1, A] =
  {
    type F[a] = State[S, a]
    def loop(n: S)(ev: VE[A, R0]): Eff[R1, A] = {
      def handle(r: F[VE[A, R0]]): Eff[R1, A] = r match {
        case Get(k   ) => loop(n)(k( n))
        case Set(k, n) => loop(n)(k(()))
      }
      ev match {
        case x: Val[A, _] => Eff(x.value)
        case E(u) =>
          Eff.handleRelay(loop(n), handle)(u)
      }
    }
    loop(n)(Eff.admin(ex))
  }

}
