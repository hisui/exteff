package exteff

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe.{typeTag, TypeTag}

trait ⊆[X <: HList, Y <: HList]

trait Subset_0 {
  implicit def case2[X <: HList] = new ⊆[X, X] {}
  implicit def case3[X <: HList, Y <: HList, Z <: HList]
  (implicit ev1: ⊆[X, Y], ev2: ⊆[Y, Z]) = new ⊆[X, Z] {}
}

object ⊆ extends Subset_0 {
  import HList._
  implicit def case0[Y <: HList] = new ⊆[`[]`, Y] {}
  implicit def case1[
    H1, T1 <: HList,
    H2, T2 <: HList]
  (implicit ev1: Member[H1, H2::T2],
            ev2: ⊆[T1, H2::T2]) = new ⊆[H1::T1, H2::T2] {}
}

trait ∪[X <: HList, Y <: HList, Z <: HList] <: ⊆[X, Z]

trait Union_0 {
  implicit def case0[X <: HList] = new ∪[X, X, X] {}
  implicit def case1[X <: HList, Y <: HList, Z <: HList, A <: HList]
  (implicit ev0: N.=/=[X, Y],
   ev1: HList.Append[X, Y, A],
   ev2: HList.Unique[A, Z]) = new ∪[X, Y, Z] {}
}

object ∪ extends Union_0 {
  //implicit def case2[X <: HList] = new ∪[`[]`, X, X] {}
  implicit def case3[X <: HList] = new ∪[X, `[]`, X] {}
}

sealed trait Union[S <: HList, V] {

  def as[A: TypeTag]: Option[A]

  def fmap[V2: TypeTag](f: V => V2): Union[S, V2]

  def decompose[F[_], A, S2 <: HList]
  (implicit tag: TypeTag[F[A]], ev: Union.Remove[F, S, S2]): Either[Union[S2, V], F[A]] =
    as[F[A]].map(Right(_)) getOrElse Left(asInstanceOf[Union[S2, V]])

  def widen[X <: HList](implicit ev: ⊆[S, X]): Union[X, V]

}

object Union {

  type |:[F[_], T <: HList] = ::[F[Unit], T]

  type Member[F[_], R <: HList] = ⊆[F|:`[]`, R]
  type Remove[F[_]
    , R0 <: HList
    , R1 <: HList] = HList.Remove[F[Unit], R0, R1]

  def apply[F[_]: Functor, V: TypeTag](e: F[V])
  (implicit tag: TypeTag1[F]): Union[F|:`[]`, V] = wrap(e)

  private def wrap[F[_]: Functor, V: TypeTag, S <: HList](e: F[V])
  (implicit tag: TypeTag1[F], ev0: ⊆[F|:`[]`, S]): Union[S, V] = new Union[S, V] {

    def as[A: TypeTag]: Option[A] =
      if (typeTag[A].tpe =:= tag.apply[V].tpe)
           Some(e.asInstanceOf[A])
      else None

    def fmap[V2: TypeTag](f: V => V2): Union[S, V2] = wrap(implicitly[Functor[F]].fmap(f)(e))

    def widen[X <: HList](implicit ev: ⊆[S, X]): Union[X, V] = wrap(e)

    override def toString(): String = s"Union($e)"
  }

  implicit def toSuper[X <: HList, Y <: HList, V](o: Union[X, V])
  (implicit ev: X ⊆ Y): Union[Y, V] = o.widen
}

