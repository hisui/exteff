package exteff

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.compat._

trait TypeTag1[F[_]] {
  def apply[A: TypeTag]: TypeTag[F[A]]
}

object TypeTag1 {

  implicit def tag[F[_]]
  (implicit tag: TypeTag[F[Unit]]): TypeTag1[F] = new TypeTag1[F] {
    def apply[A: TypeTag]: TypeTag[F[A]] = tag.tpe match {
      case TypeRef(a, b, List(_)) =>
        typeToTypeTag(TypeRef(a, b, List(typeTag[A].tpe)), tag.mirror)
    }
  }

  // http://stackoverflow.com/a/25691045/2681195
  def typeToTypeTag[T](tpe: Type, mirror: reflect.api.Mirror[reflect.runtime.universe.type]): TypeTag[T] =
    TypeTag(mirror, new reflect.api.TypeCreator {
      def apply[U <: reflect.api.Universe with Singleton](m: reflect.api.Mirror[U]) = tpe.asInstanceOf[U#Type]
    })
}
