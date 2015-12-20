package exteff

import scala.reflect.runtime.universe.TypeTag

import exteff.Union._

sealed trait Iteratee[I, O, A]
case class Done[I, O, A](k: O => A, out: O) extends Iteratee[I, O, A]
case class Cont[I, O, A](k: Input[I] => A) extends Iteratee[I, O, A]

sealed trait Step[-I, +O, +A]
object Step {
  case object None extends Step[Any, Nothing, Nothing]
  case class Done[I, O, A](after: A, out: O) extends Step[I, O, A]
  case class Cont[I, O, A](k: Input[I] => A) extends Step[I, O, A]
}

sealed trait Input[+A]
object Input {
  case class El[A](value: A) extends Input[A]
  case object Eof extends Input[Nothing]
}

object Iteratee {

  type Apply[I, O] = ({ type f[a] = Iteratee[I, O, a] })

  implicit def functor[I, O] = new Functor[Apply[I, O]#f] {
    def fmap[A, B](f: A => B)(ma: Iteratee[I, O, A]): Iteratee[I, O, B] = ma match {
      case Cont(k)    => Cont(i => f(k(i)))
      case Done(k, o) => Done(o => f(k(o)), o)
    }
  }

  def step[I: TypeTag, O: TypeTag] = {
    Eff.send(new New[Input[I], Iteratee[I, O, Unit] ::`[]`] {
      def apply[W: TypeTag, R2 <: HList : TypeTag]
      (k: Input[I] => VE[W, R2])(implicit ev: ⊆[Iteratee[I, O, Unit] ::`[]`, R2]): Union[R2, VE[W, R2]] = Union[Apply[I, O]#f, VE[W, R2]] { Cont(k) }
    })
  }

  // TODO for Eof
  def head[I: TypeTag] = for {
    in <- step[I, I]
    el  <- in match { case Input.El(a) => Eff(a) }
  } yield el

  def done[I: TypeTag, O: TypeTag](o: O) = {
    Eff.send(new New[O, Iteratee[I, O, Unit] :: `[]`] {
      def apply[W: TypeTag, R2 <: HList : TypeTag]
      (k: O => VE[W, R2])(implicit ev: ⊆[Iteratee[I, O, Unit] ::`[]`, R2]): Union[R2, VE[W, R2]] =
        Union[Apply[I, O]#f, VE[W, R2]] {
          Done(k, o)
        }
    })
  }

  def push[
    A: TypeTag,
    I: TypeTag,
    O: TypeTag,
    R0 <: HList : TypeTag,
    R1 <: HList : TypeTag
  ]
  (step: Step[I, O, Eff[R0, A]], value: I)
  (implicit ev0: Remove[Apply[I, O]#f, R0, R1]): Eff[R1, Step[I, O, Eff[R0, A]]] =
    step match {
      case Step.Cont(k) => pack[A, I, O, R0, R1](k(Input.El(value)))
    }

  def pack[
    A: TypeTag,
    I: TypeTag,
    O: TypeTag,
    R0 <: HList : TypeTag,
    R1 <: HList : TypeTag
  ]
  (ex: Eff[R0, A])
  (implicit ev1: Remove[Apply[I, O]#f, R0, R1]): Eff[R1, Step[I, O, Eff[R0, A]]] =
  {
    type F[a] = Apply[I, O]#f[a]
    def handle(r: Either[F[VE[A, R0]], A]): Eff[R1, Step[I, O, Eff[R0, A]]] = r match {
      case Left(Cont(k))    => Eff(Step.Cont[I, O, Eff[R0, A]](i => Eff.wrap[A, R0](k(i))))
      case Left(Done(k, o)) => Eff(Step.Done[I, O, Eff[R0, A]](Eff.wrap[A, R0](k(o)), o))
      case Right(a) =>
        Eff(Step.None)
    }
    Eff.handleNext(handle)(ex)
  }

}
