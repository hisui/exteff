package exteff

object N {

  trait Expr

  abstract class Num(val raw: Int) extends Expr

  object `0` extends Num(0)

  case class s[E](n: Int) extends Num(n)

  if (false) (1 to 22).foreach { i =>
    println(
      """
        |type `<i>` = s[`<j>`]
      """.stripMargin.trim
        .replace("<i>", ""+ (i))
        .replace("<j>", ""+ (i - 1)))
  }

  type  `0` = `0`.type
  type  `1` = s[`0`]
  type  `2` = s[`1`]
  type  `3` = s[`2`]
  type  `4` = s[`3`]
  type  `5` = s[`4`]
  type  `6` = s[`5`]
  type  `7` = s[`6`]
  type  `8` = s[`7`]
  type  `9` = s[`8`]
  type `10` = s[`9`]
  type `11` = s[`10`]
  type `12` = s[`11`]
  type `13` = s[`12`]
  type `14` = s[`13`]
  type `15` = s[`14`]
  type `16` = s[`15`]
  type `17` = s[`16`]
  type `18` = s[`17`]
  type `19` = s[`18`]
  type `20` = s[`19`]
  type `21` = s[`20`]
  type `22` = s[`21`]

  trait #+# [A <: Num, B <: Num, Z <: Num] {
    type Out = Z
    def apply(a: A, b: B): Z
  }

  object #+# {
    implicit def case0[A <: Num, Z <: Num] = new #+# [A, `0`, A]() {
      def apply(a: A, b:`0`): A = a
    }
    implicit def case1[A <: Num, B <: Num, Z <: Num]
    (implicit add: #+#[A, B, Z]) = new #+# [A, s[B], s[Z]]() {
      def apply(a: A, b: s[B]): s[Z] = s[Z](a.raw + b.raw)
    }
  }

  trait #*# [A <: Num, B <: Num, Z <: Num] {
    type Out = Z
    def apply(a: A, b: B): Z
  }

  object #*# {
    implicit def case0[A <: Num] = new #*# [`0`, `0`, `0`]() {
      def apply(a:`0`, b:`0`):`0` = `0`
    }
    implicit def case1[A <: Num] = new #*# [s[A], `0`, `0`]() {
      def apply(a: s[A], b:`0`):`0` = `0`
    }
    implicit def case2[B <: Num] = new #*# [`0`, s[B], `0`]() {
      def apply(a:`0`, b: s[B]):`0` = `0`
    }
    implicit def case3[A <: Num, B <: Num, Y <: Num, Z <: Num]
    (implicit mul: #*#[s[A], B, Y], add: #+#[Y, s[A], s[Z]]) = new #*# [s[A], s[B], s[Z]]() {
      def apply(a: s[A], b: s[B]): s[Z] = s[Z](a.raw * b.raw)
    }
    implicit def case4[A <: Num, B <: Num, Y <: Num, Z <: Num]
    (implicit add: #+#[s[A], Y, s[Z]], mul: #*#[s[A], B, Y]) = new #*# [s[A], s[B], s[Z]]() {
      def apply(a: s[A], b: s[B]): s[Z] = s[Z](a.raw * b.raw)
    }
  }

  trait + [A <: Expr, B <: Expr] extends Expr
  trait - [A <: Expr, B <: Expr] extends Expr
  trait * [A <: Expr, B <: Expr] extends Expr
  trait / [A <: Expr, B <: Expr] extends Expr

  trait as[E <: Expr, Z <: Num]

  object as {
    implicit def id[N <: Num] = new as[N, N]() {}
    implicit def + [N <: Num
    , A <: Expr, A2 <: Num
    , B <: Expr, B2 <: Num]
    (implicit as1: A as A2, as2: B as B2, add: #+#[A2, B2, N]) = new as[A `+` B, N]() {
    }
    implicit def - [N <: Num
    , A <: Expr, A2 <: Num
    , B <: Expr, B2 <: Num]
    (implicit as1: A as A2, as2: B as B2, add: #+#[B2, N, A2]) = new as[A `-` B, N]() {
    }
    implicit def * [N <: Num
    , A <: Expr, A2 <: Num
    , B <: Expr, B2 <: Num]
    (implicit as1: A as A2, as2: B as B2, mul: #*#[A2, B2, N]) = new as[*[A, B], N]() {
    }
    implicit def / [N <: Num
    , A <: Expr, A2 <: Num
    , B <: Expr, B2 <: Num]
    (implicit as1: A as A2, as2: B as B2, mul: #*#[B2, N, A2]) = new as[A `/` B, N]() {
    }
  }

  trait < [A <: Num, B <: Num]

  object < {
    implicit def case0[X <: Num] = new < [`0`, s[X]] {}
    implicit def case1[A <: Num, B <: Num](implicit ev1: A < B) = new < [s[A], s[B]] {}
  }

  // low priority implicits for Eq
  trait Eq0 {
    implicit def case0[A, B] = new Eq[A, B, `0`] {}
  }

  trait Eq[A, B, C]

  object Eq extends Eq0 {
    implicit def case1[A] = new Eq[A, A, `1`] {}
  }

  trait =/=[A, B]

  object =/= {
    implicit def case0[A, B, C](implicit eq: Eq[A, B, C], wt: C =:= `0`) = new =/=[A, B] {}
  }

}
