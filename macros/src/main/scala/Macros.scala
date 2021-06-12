package dev.thinkharder

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import scala.quoted.*


trait DeriveSchema[T]:
  def derive: Schema[T]

object DeriveSchema:
  import Schema.*

  def apply[T](using Schema[T]): Schema[T] = summon[Schema[T]]

  // given lzyOptional[A](using s: Schema[A]): Lazy[Schema[Option[A]]] = defer(Optional(s))

  // given lzyList[A](using s: Schema[A]): Lazy[Schema[List[A]]] = defer(ListSchema(s))

  // given [A,T <: Tuple] (using a: Lazy[Schema[A]], t: Lazy[Schema[T]]): Lazy[Schema[A *: T]] = 
  //   defer((a.value <*> t.value).map(_ *: _))

  given Schema[String] = StringSchema

  given [A] (using s: Schema[A]): Schema[Option[A]] = Optional(s)

  given [A] (using s: Schema[A]): Schema[List[A]] = ListSchema(s)

  inline def gen[T](using m: Mirror.Of[T]): Schema[T] =
    inline m match 
      case s: Mirror.SumOf[T] => deriveSum0(s)
      case p: Mirror.ProductOf[T] => deriveProduct0(p)
    
  inline given derived[T](using Mirror.Of[T]): Schema[T] = gen[T]

  def deriveSum[T](m: Mirror.SumOf[T], schemas: => List[Schema[_]], labels: => List[String]): Schema[T] =
    labels.zip(schemas).map((label,schema) => Case[Any,T](label,schema.asInstanceOf[Schema[Any]])) match 
      case List(c) => Enum1(c)
      case List(c1,c2) => Enum2(c1,c2)
      case List(c1,c2,c3) => Enum3(c1,c2,c3)
      case cases => EnumN(cases)
  


  def deriveProduct[T](m: Mirror.ProductOf[T], schemas: => List[Schema[_]], labels: => List[String]): Schema[T] =
    Record(
      labels.zip(schemas).map {
        case (label,schema) => Field(label,schema)
      }
    )
    

  inline def deriveProduct0[T](p: Mirror.ProductOf[T]): Schema[T] = 
    deriveProduct(p,summonSchemas[p.MirroredElemTypes],summonLabels[p.MirroredElemLabels])

  inline def deriveSum0[T](s: Mirror.SumOf[T]): Schema[T] =
    deriveSum(s,summonSchemas[s.MirroredElemTypes],summonLabels[s.MirroredElemLabels])


  // inline def deriveCases[FieldTypes <: Tuple, Labels <: Tuple]: List[Field[_]] =
  //   inline erasedValue[(FieldTypes,Labels)] match 
  //     case _: (EmptyTuple,_) => Nil
  //     case _: (f *: ftail, l *: ltail) =>
  //       summonFrom {
  //         case s: Schema[`f`] => Field(constValue[l].asInstanceOf[String], s) :: deriveCases[ftail,ltail]
  //         case sum: Mirror.SumOf[T] => Field(constValue[l].asInstanceOf[String], ) 
  //       }

  inline def summonLabels[Labels <: Tuple]: List[String] =
    inline erasedValue[Labels] match 
      case _: EmptyTuple => Nil
      case _: (l *: tail) => constValue[l].asInstanceOf[String] :: summonLabels[tail]

  inline def summonSchemas[Subtypes <: Tuple]: List[Schema[_]] =
    inline erasedValue[Subtypes] match 
      case _: EmptyTuple => Nil
      case _: (s *: tail) => 
        defer(summonInline[Schema[s]]) :: summonSchemas[tail]
        // summonFrom {
        //   case s: Schema[`s`] => defer(s) :: summonSchemas[tail]
        //   case p: Mirror.ProductOf[`s`] => defer(deriveProduct0(p)) :: summonSchemas[tail]
        //   case sum: Mirror.SumOf[`s`] => defer(deriveSum0(sum)) :: summonSchemas[tail]
        // }
end DeriveSchema
