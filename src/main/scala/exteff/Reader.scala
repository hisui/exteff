package exteff

import scala.reflect.runtime.universe.TypeTag

import Union._
import Eff.Eff1

trait Reader[I] { lhs =>

  case class F[A](k: I => A)

  implicit def functor = new Functor[F] {
    def fmap[A, B](f: A => B)(ma: F[A]): F[B] = F(n => f(ma.k(n)))
  }

  def ask: Eff1[F, I] = Eff.send(new New[I, F |: `[]`]
  {
    def apply[W: TypeTag, R2 <: HList : TypeTag]
    (k: I => VE[W, R2])(implicit ev: Subset[F |: `[]`, R2]): Union[R2, VE[W, R2]] =
      Union[F, VE[W, R2]](F(k))
  })

  def run[A: TypeTag
    , R <: HList : TypeTag
    , S <: HList : TypeTag](ex: Eff[R, A], n: I)
  (implicit ev1: Remove[F, R, S]): Eff[S, A] =
  {
    def loop(ev: VE[A, R]): Eff[S, A] = ev match {
      case x: Val[A, _] => Eff(x.value)
      case E(u) =>
        Eff.handleRelay(u)(loop, { r: F[VE[A, R]] => loop(r.k(n)) })
    }
    loop(Eff.admin(ex))
  }

}
