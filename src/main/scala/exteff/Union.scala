package exteff

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe.{typeTag, TypeTag}

trait Subset[X <: HList, Y <: HList]

trait Case2 {
  implicit def case2[X <: HList] = new Subset[X, X] {}
  implicit def case3[X <: HList, Y <: HList, Z <: HList]
  (implicit ev1: Subset[X, Y], ev2: Subset[Y, Z]) = new Subset[X, Z] {}
}

object Subset extends Case2 {
  import HList._

  implicit def case0[Y <: HList] = new Subset[`[]`, Y] {}
  implicit def case1[
    H1, T1 <: HList,
    H2, T2 <: HList]
  (implicit ev1: Member[H1, H2::T2],
            ev2: Subset[T1, H2::T2]) = new Subset[H1::T1, H2::T2] {}
}

trait Fusion[X <: HList, Y <: HList, Z <: HList] <: Subset[X, Z]

object Fusion {
  implicit def case0[X <: HList, Y <: HList, Z <: HList, A <: HList]
  (implicit ev0: N.=/=[X, Y],
            ev1: HList.Append[X, Y, A],
            ev2: HList.Unique[A, Z]) = new Fusion[X, Y, Z] {}

  implicit def case1[X <: HList] = new Fusion[X, X, X] {}
  implicit def case2[X <: HList] = new Fusion[`[]`, X, X] {}
  implicit def case3[X <: HList] = new Fusion[X, `[]`, X] {}
}

trait Remove[F[_], X <: HList, Y <: HList] extends Union.Member[F, X]

object Remove {
  implicit def case0[F[_], X <: HList, Y <: HList]
  (implicit ev1: HList.Remove[F[Unit], X, Y]) = new Remove[F, X, Y] {}
}

sealed trait Union[S <: HList, V] {

  def as[A: TypeTag]: Option[A]

  def fmap[V2: TypeTag](f: V => V2): Union[S, V2]

  // TODO remove asInstanceOf
  def decompose[F[_], A, S2 <: HList]
  (implicit tag: TypeTag[F[A]], ev: Remove[F, S, S2]): Either[Union[S2, V], F[A]] =
    as[F[A]].map(Right(_)) getOrElse Left(asInstanceOf[Union[S2, V]])

  def widen[X <: HList](implicit ev: Subset[S, X]): Union[X, V]
}

object Union {

  type |:[F[_], T <: HList] = ::[F[Unit], T]

  type Member[F[_], R <: HList] = Subset[F|:`[]`, R]

  def apply[F[_]: Functor, V: TypeTag](e: F[V])
  (implicit tag: TypeTag1[F]): Union[F|:`[]`, V] = wrap(e)

  private def wrap[F[_]: Functor, V: TypeTag, S <: HList](e: F[V])
  (implicit tag: TypeTag1[F], ev0: Subset[F|:`[]`, S]): Union[S, V] = new Union[S, V] {

    def as[A: TypeTag]: Option[A] =
      if (typeTag[A] == tag.apply[V])
           Some(e.asInstanceOf[A])
      else None

    def fmap[V2: TypeTag](f: V => V2): Union[S, V2] = wrap(implicitly[Functor[F]].fmap(f)(e))

    def widen[X <: HList](implicit ev: Subset[S, X]): Union[X, V] = wrap(e)

  }

  implicit def toSuper[X <: HList, Y <: HList, V](o: Union[X, V])
  (implicit ev: Subset[X, Y]): Union[Y, V] = o.widen
}

