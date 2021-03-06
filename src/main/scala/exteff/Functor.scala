package exteff

import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[A, B](f: A => B)(fa: F[A]): F[B]
}
