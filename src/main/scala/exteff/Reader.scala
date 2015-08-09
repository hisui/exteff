package exteff

import scala.reflect.runtime.universe.TypeTag

import Union._
import Eff.Eff1

case class Reader[I, A](k: I => A)
object Reader {

  implicit def functor[I] = new Functor[({ type z[a] = Reader[I, a] })#z] {
    def fmap[A, B](f: A => B)(ma: Reader[I, A]): Reader[I, B] = Reader(n => f(ma.k(n)))
  }

  def ask[I: TypeTag] = Eff.send(new New[I, ({ type z[a] = Reader[I, a] })#z |: `[]`]
  {
    def apply[W: TypeTag, R2 <: HList : TypeTag](k: I => VE[W, R2])
    (implicit ev: Subset[({ type z[a] = Reader[I, a] })#z|:`[]`, R2]): Union[R2, VE[W, R2]] =
      Union[({ type z[a] = Reader[I, a] })#z, VE[W, R2]](Reader(k))
  })

  def run[A: TypeTag, I: TypeTag
    , R0 <: HList : TypeTag
    , R1 <: HList : TypeTag](ex: Eff[R0, A], n: I)
  (implicit ev1: Remove[({ type z[a] = Reader[I, a] })#z, R0, R1]): Eff[R1, A] =
  {
    type ReaderF[a] = Reader[I, a]
    def loop(ev: VE[A, R0]): Eff[R1, A] = ev match {
      case x: Val[A, _] => Eff(x.value)
      case E(u) =>
        Eff.handleRelay(loop, { r: ReaderF[VE[A, R0]] => loop(r.k(n)) })(u)
    }
    loop(Eff.admin(ex))
  }

}
