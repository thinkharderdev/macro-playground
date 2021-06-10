package dev.thinkharder

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import scala.quoted.*

trait DeriveSchema[T]:
  def derive: Schema[T]

object DeriveSchema:
  def apply[T](using DeriveSchema[T]): Schema[T] = summon[DeriveSchema[T]].derive

  inline def instance[A](schema: => Schema[A]): DeriveSchema[A] =
    new DeriveSchema[A]:
      override def derive: Schema[A] = schema

  given DeriveSchema[String] = instance(Schema.StringSchema)

  given [A] (using s: Schema[A]): DeriveSchema[Option[A]] =
    instance(Schema.Optional(s))

  inline def gen[T](using m: Mirror.Of[T]): DeriveSchema[T] =
    new DeriveSchema[T]:
      override def derive: Schema[T] =
        val instances = summonAll[m.MirroredElemTypes]
        inline m match
          case s: Mirror.SumOf[T] => deriveSum[T](s,instances)
          case p: Mirror.ProductOf[T] => deriveProduct(p,instances)


  inline given derived[T](using Mirror.Of[T]): DeriveSchema[T] = gen[T]

  inline def zipFields[Labels <: Tuple](schemas: => List[DeriveSchema[_]]): List[Field[_]] =
    inline erasedValue[Labels] match
      case _: EmptyTuple => Nil
      case _: (l *: tail) =>
        Field(constValue[l].asInstanceOf[String], Lazy(schemas.head.derive)) :: zipFields[tail](schemas.tail)

  inline def zipCases[T, Labels <: Tuple](schemas: => List[DeriveSchema[_]]): List[Case[_,T]] =
    inline erasedValue[Labels] match
      case _: EmptyTuple => Nil
      case _: (l *: tail) =>
        Case(constValue[l].asInstanceOf[String], Lazy(schemas.head.derive)) :: zipCases[T,tail](schemas.tail)

  inline def deriveSum[T](m: Mirror.SumOf[T], schemas: => List[DeriveSchema[_]]): Schema[T] =
    Lazy(
      inline zipCases[T,m.MirroredElemLabels](schemas) match
        case List(c) => Enum1(c)
        case List(c1,c2) => Enum2(c1,c2)
        case List(c1,c2,c3) => Enum3(c1,c2,c3)
        case cs => EnumN(cs)
    )

  inline def deriveProduct[T](m: Mirror.ProductOf[T], schemas: => List[DeriveSchema[_]]): Schema[T] =
    Lazy(
      inline zipFields[m.MirroredElemLabels](schemas) match
        case ls => Record(ls)
    )


  inline def summonAll[T <: Tuple]: List[DeriveSchema[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: tail) =>
        summonInline[DeriveSchema[t]] :: summonAll[tail]

end DeriveSchema
