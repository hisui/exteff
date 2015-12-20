package exteff

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag

import Union._

sealed trait VE[W, R <: HList]

case class Val[W, R <: HList](value: W) extends VE[W, R]

case class E[W, R <: HList]
(union: Union[R, VE[W, R]]) extends VE[W, R]

object VE {
  implicit def toSuper[W: TypeTag
  , R0 <: HList : TypeTag
  , R1 <: HList : TypeTag](r: VE[W, R0])
  (implicit ev: R0 ⊆ R1): VE[W, R1] = if (false) r.asInstanceOf[VE[W, R1]]
  else r match {
    case Val(a) => Val(a)
    case   E(a) =>   E(a.fmap(e => e : VE[W, R1]))
  }
}

trait Eff[R0 <: HList, +A] { lhs =>

  def run[W: TypeTag, R1 <: HList : TypeTag]
  (k: A => VE[W, R1])(implicit ev: R0 ⊆ R1): VE[W, R1]

  def flatMap[B, R1 <: HList, R3 <: HList](f: A => Eff[R1, B])
  (implicit ev0: ∪[R0, R1, R3]
    , ev1: R0 ⊆ R3
    , ev2: R1 ⊆ R3): Eff[R3, B] = new Eff[R3, B]
  {
    def run[W: TypeTag, R4 <: HList : TypeTag]
    (k: B => VE[W, R4])(implicit ev: R3 ⊆ R4): VE[W, R4] = lhs.run(a => f(a).run(b => k(b)))
  }

  def map[B](f: A => B): Eff[R0, B] = flatMap(a => Eff(f(a)))

}

trait New[A, R <: HList] {
  def apply[W: TypeTag, R2 <: HList : TypeTag](k: A => VE[W, R2])
  (implicit ev: R ⊆ R2): Union[R2, VE[W, R2]]
}

object Eff {

  type Eff1[F[_], A] = Eff[F |: `[]`, A]

  def apply[A](a: A): Eff[`[]`, A] = new Eff[`[]`, A] {
    def run[W: TypeTag, R <: HList : TypeTag]
    (k: A => VE[W, R])(implicit ev: `[]` ⊆ R): VE[W, R] = k(a)
  }

  def send[R <: HList, A](r: New[A, R]): Eff[R, A] = new Eff[R, A] {
    def run[W: TypeTag, R2 <: HList : TypeTag]
    (k: A => VE[W, R2])(implicit ev: R ⊆ R2): VE[W, R2] = E[W, R2](r(k))
  }

  def admin[R <: HList : TypeTag, A: TypeTag](ex: Eff[R, A])
  (implicit ev: R ⊆ R): VE[A, R] = ex.run(a => Val[A, R](a))

  def run[A: TypeTag](ex: Eff[`[]`, A]): A = admin(ex) match { case Val(a) => a }

  def handleRelay[F[_]: Functor : TypeTag1
    , A: TypeTag
    , V: TypeTag
    , R0 <: HList : TypeTag
    , R1 <: HList : TypeTag
  ]
  (loop: V => Eff[R1, A], handle: F[V] => Eff[R1, A])(r: Union[R0, V])
  (implicit ev: Remove[F, R0, R1]): Eff[R1, A] =
    r.decompose(implicitly[TypeTag1[F]].apply[V], ev) match {
      case Right(fa) => handle(fa)
      case  Left(r2) =>
        send(new New[V, R1] {
          def apply[W: TypeTag, R <: HList : TypeTag](k: V => VE[W, R])(implicit ev: R1 ⊆ R): Union[R, VE[W, R]] = r2.fmap(k)
        }).flatMap(loop)
    }

  def handleNext[F[_]: Functor : TypeTag1
  , A: TypeTag
  , V: TypeTag
  , R0 <: HList : TypeTag
  , R1 <: HList : TypeTag
  ]
  (handle0: Either[F[VE[V, R0]], V] => Eff[R1, A])(ex: Eff[R0, V])
  (implicit ev: Remove[F, R0, R1]): Eff[R1, A] = {
    def handle(r: F[VE[V, R0]]): Eff[R1, A] = handle0(Left(r))
    def loop(r: VE[V, R0]): Eff[R1, A] = r match {
      case Val(a) => handle0(Right(a))
      case E(u) => handleRelay(loop, handle)(u)
    }
    loop(admin(ex))
  }

  implicit def toSuper[R0 <: HList, R1 <: HList, A]
  (ex: Eff[R0, A])(implicit ev0: R0 ⊆ R1): Eff[R1, A] = if (false) ex.asInstanceOf[Eff[R1, A]]
    else {
      new Eff[R1, A] {
        def run[W: TypeTag, R2 <: HList : TypeTag]
        (k: A => VE[W, R2])(implicit ev: R1 ⊆ R2): VE[W, R2] = ex.run(k)
      }
    }

  def wrap0[A, R1 <: HList](e: E[A, R1]): Eff[R1, A] = new Eff[R1, A] {
    def run[W: TypeTag, R2 <: HList : TypeTag](k: A => VE[W, R2])(implicit ev: R1 ⊆ R2): VE[W, R2] = {
      def loop(e: E[A, R1]): Union[R1, VE[W, R2]] = e.union.fmap {
        case Val(a)      => k(a)
        case e: E[A, R1] => E(loop(e))
      }
      E[W, R2](loop(e))
    }
  }

  def wrap[A, R1 <: HList](ev: VE[A, R1]): Eff[R1, A] = ev match {
    case v: Val[A, R1] => Eff(v.value)
    case e:   E[A, R1] => wrap0(e)
  }

}
