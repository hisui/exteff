package exteff

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.compat._

class TypeTag1[F[_]](tag: TypeTag[F[Unit]]) {
  def apply[A: TypeTag]: TypeTag[F[A]] = tag.tpe match {
    case TypeRef(a, b, l) =>
      TypeTag1.typeToTypeTag(TypeRef(a, b, l.init :+ typeTag[A].tpe), tag.mirror)
  }
}

object TypeTag1 {

  implicit def case0[F[_]]
  (implicit tag: TypeTag[F[Unit]]) = new TypeTag1[F](tag)

  implicit def case1[F[_, _], X]
  (implicit tag: TypeTag[F[X, Unit]]) = new TypeTag1[({ type z[a] = F[X, a] })#z](tag)

  implicit def case2[F[_, _, _], X, Y]
  (implicit tag: TypeTag[F[X, Y, Unit]]) = new TypeTag1[({ type z[a] = F[X, Y, a] })#z](tag)

  // http://stackoverflow.com/a/25691045/2681195
  def typeToTypeTag[T](tpe: Type, mirror: reflect.api.Mirror[reflect.runtime.universe.type]): TypeTag[T] =
    TypeTag(mirror, new reflect.api.TypeCreator {
      def apply[U <: reflect.api.Universe with Singleton](m: reflect.api.Mirror[U]) = tpe.asInstanceOf[U#Type]
    })
}
