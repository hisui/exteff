package exteff

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.language.higherKinds

sealed trait HList {
  def foldl[X](value: X)(f: (X, Any) => X): X = value
}

sealed trait `[]` extends HList

object `[]` extends `[]` {
  override def toString() = "[]"
}

case class ::[A, B <: HList](head: A, tail: B) extends HList {

  override def toString() = tail.foldl("[" + head)(_ + "," + _ ) + "]"

  override def foldl[X]
  (value: X)(f: (X, Any) => X): X = tail.foldl(f(value, head))(f)
}

object HList {

  import N._

  trait Reverse[Y,X <: HList,Z <: HList] {
    def apply(o: Y, acc: X): Z
  }

  object Reverse {
    implicit def case0[X <: HList] = new Reverse[`[]`, X, X] {
      def apply(o:`[]`, acc: X): X = acc
    }

    implicit def case1[H, T <: HList, X <: HList, Z <: HList]
    (implicit ev: Reverse[T, H::X, Z]) = new Reverse[H::T, X, Z] {
      def apply(o: H::T, acc: X): Z = ev(o.tail, o.head :: acc)
    }
  }

  trait Member[E, X <: HList]

  object Member {

    implicit def case1[E, T <: HList] = new Member[E, E::T] {}

    implicit def case2[E, H, T <: HList]
    (implicit ev1: E =/= H, ev2: Member[E, T]) = new Member[E, H::T] {}
  }

  /*
  trait Forall[P[_], X <: HList]

  object Forall {
    implicit def case0[P[_]] = new Forall[P, `[]`] {}

    implicit def case1[P[_], H, T <: HList]
    (implicit ev1: P[H], ev2: Forall[P, T]) = new Forall[P, H::T] {}
  }
  */

  trait Unique[X <: HList, Z <: HList] {
    def apply(a: X): Z    
  }

  object Unique {
    implicit def case0 = new Unique[`[]`, `[]`] {
      def apply(a: `[]`): `[]` = a
    }

    implicit def case1[H, T <: HList, Y <: HList, Z <: HList]
    (implicit ev1: Remove[H, T, Y], ev2: Unique[Y, Z]) = new Unique[H::T, H::Z] {
      def apply(a: H::T): H::Z = a.head :: ev2(ev1(a.head, a.tail))
    }
  }

  trait Remove[E, X <: HList, Z <: HList] {
    def apply(e: E, a: X): Z
  }

  object Remove {

    implicit def case0[E] = new Remove[E, `[]`, `[]`] {
      def apply(e: E, a: `[]`): `[]` = `[]`
    }

    implicit def case1[E, T <: HList, Z <: HList]
    (implicit ev: Remove[E, T, Z]) = new Remove[E, E::T, Z] {
      def apply(e: E, a: E::T): Z = ev(e, a.tail)
    }

    implicit def case2[E, H, T <: HList, Z <: HList]
    (implicit ev1: E =/= H, ev2: Remove[E, T, Z]) = new Remove[E, H::T, H::Z] {
      def apply(e: E, a: H::T): H::Z = a.head :: ev2(e, a.tail)
    }
  }

  trait Append[Y <: HList, X <: HList, Z <: HList] {
    def apply(a: Y, b: X): Z
  }

  object Append {
    implicit def case0[X <: HList] = new Append[`[]`, X, X] {
      def apply(a:`[]`, b: X): X = b
    }

    implicit def case1[H, T <: HList, X <: HList, Z <: HList]
    (implicit ev: Append[T, X, Z]) = new Append[H::T, X, H::Z] {
      def apply(a: H::T, b: X): H::Z = a.head :: ev(a.tail, b)
    }
  }

  trait Head[X <: HList, Z] {
    type Out = Z
    def apply(o: X): Z
  }

  object Head {
    implicit def case0[H, T <: HList] = new Head[H::T, H] {
      override type Out = H
      def apply(o: H::T): H = o.head
    }
  }

  trait Tail[X <: HList, Z <: HList] {
    type Out = Z
    def apply(o: X): Z
  }

  object Tail {
    implicit def case0[H, T <: HList] = new Tail[H::T, T] {
      def apply(o: H::T): T = o.tail
    }
  }

  trait Last[X <: HList, Z] {
    type Out = Z
    def apply(o: X): Z
  }

  object Last {
    implicit def case0[A <: HList, B <: HList, C]
    (implicit reverse: Reverse[A, `[]`, B], head: Head[B, C]) = new Last[A, C] {
      override type Out = C
      def apply(o: A): C = head(reverse(o, `[]`))
    }
  }

  trait Take[X <: HList, N <: Num, Z <: HList] {
    type Out = Z
    def apply(o: X): Z
  }

  object Take {
    implicit def case0[Z <: HList] = new Take[Z, `0`, `[]`] {
      def apply(o: Z): `[]` = `[]`
    }
    implicit def case1[H, T <: HList, Z <: HList, I <: Num, J <: Num]
    (implicit ev1: (I - `1`) as J, ev2: Take[T, J, Z]) = new Take[H::T, I, H::Z] {
      def apply(o: H::T): H::Z = o.head :: ev2(o.tail)
    }
  }

  trait Drop[X <: HList, N <: Num, Z <: HList] {
    type Out = Z
    def apply(o: X): Z
  }

  object Drop {
    implicit def case0[X <: HList] = new Drop[X, `0`, X] {
      def apply(o: X): X = o
    }
    implicit def case1[H, T <: HList, Z <: HList, I <: Num, J <: Num]
    (implicit ev1: (I - `1`) as J, ev2: Drop[T,J,Z]) = new Drop[H::T, I, Z] {
      def apply(o: H::T): Z = ev2(o.tail)
    }
  }

  trait Length[X <: HList, Z <: Num] {
    def apply(o: X): Int
  }

  object Length {
    implicit def case0 = new Length[`[]`, `0`] {
      def apply(o: `[]`): Int = 0
    }
    implicit def case1[H, T <: HList, I <: Num, J <: Num]
    (implicit ev1: Length[T, J], ev2: (J + `1`) as I) = new Length[H::T, I] {
      def apply(o: H::T): Int = ev1(o.tail) + 1
    }
  }

  implicit class HListOps[A <: HList](val raw: A) extends AnyVal {

    def ::[B](e: B) = new `::`(e, raw)

    def reverse[Z <: HList](implicit reverse: Reverse[A, `[]`, Z]): Z = reverse(raw, `[]`)

    def ++ [B <: HList, Z <: HList](b: B)(implicit append: Append[A, B, Z]): Z = append(raw, b)

    def tail[B <: HList](implicit tail: Tail[A, B]): B = tail(raw)

    def head[B](implicit head: Head[A, B]): B = head(raw)

    def last[B](implicit last: Last[A, B]): B = last(raw)

    def take[N <: Num](implicit f: Take[A, N, _ <: HList]) = f(raw)

    def drop[N <: Num](implicit f: Drop[A, N, _ <: HList]) = f(raw)

    def size(implicit f: Length[A, _]) = f(raw)

  }

}
