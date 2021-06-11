package dev.thinkharder

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import scala.quoted.*

trait DeriveSchema[T]:
  def derive: Schema[T]

trait DeriveMember[T]:
  def deriveLabel: String
  def deriveSchema: DeriveSchema[T]

object DeriveMember:
  def deferred[T](label0: () => String, deriveSchema0: () => DeriveSchema[T]) =
    new DeriveMember[T]:
      override def deriveLabel: String = label0()
      override def deriveSchema: DeriveSchema[T] = deriveSchema0()


inline def summonAsArray[T <: Tuple]: Array[Any] =
  summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = 
  inline erasedValue[T] match 
    case _: EmptyTuple => arr
    case _: (a *: b) =>
      arr(i) = summonInline[a]
      summonAsArray0[b](i+1, arr)

inline def summonValuesAsArray[T <: Tuple, E: ClassTag]: Array[E] =
  summonValuesAsArray0[T, E](0, new Array[E](constValue[Tuple.Size[T]]))

inline def summonValuesAsArray0[T, E](i: Int, arr: Array[E]): Array[E] = inline erasedValue[T] match {
  case _: EmptyTuple => arr
  case _: (a *: b) =>
    arr(i) = constValue[a & E]
    summonValuesAsArray0[b, E](i+1, arr)
}

object DeriveSchema:
  inline def instance[A](schema: => Schema[A]): DeriveSchema[A] =
    new DeriveSchema[A]:
      override def derive: Schema[A] = schema

  given [A] (using s: Schema[A]): DeriveSchema[A] = instance(s)

  inline def gen[T](using m: Mirror.Of[T]): DeriveSchema[T] =
    inline m match
      case s: Mirror.SumOf[T] => deriveSum(s)
      case p: Mirror.ProductOf[T] => deriveProduct(p)


  inline given derived[T](using Mirror.Of[T]): Schema[T] = Lazy(gen[T])
  
  inline def deriveSum[T](m: Mirror.SumOf[T]): DeriveSchema[T] =
    new DeriveSchema[T]:
      override def derive: Schema[T] =
        lazy val members = summonMembers[m.MirroredElemTypes,m.MirroredElemLabels]
        members match
          case Array(m) => Enum1(Case(m._1,Lazy(m._2)))
          case Array(m1,m2) =>
            Enum2(
              Case(m1._1,Lazy(m1._2)),
              Case(m2._1,Lazy(m2._2))
            )
          case Array(m1,m2,m3) =>
            Enum3(
              Case(m1._1,Lazy(m1._2)),
              Case(m2._1,Lazy(m2._2)),
              Case(m3._1,Lazy(m3._2))
            )
          case ms =>
            EnumN(ms.map(m => Case(m._1, Lazy(m._2))))

  type Member = (String,DeriveSchema[_])

  inline def deriveProduct[T](m: Mirror.ProductOf[T]): DeriveSchema[T] =
    new DeriveSchema[T]:
      override def derive: Schema[T] = 
        lazy val members = summonMembers[m.MirroredElemTypes,m.MirroredElemLabels]
        Record(members.map(m => Field(m._1, Lazy(m._2))))

  inline def summonMembers[Subtypes <: Tuple, Labels <: Tuple]: Array[Member] =
    summonMembers0[Subtypes,Labels](new Array[Member](constValue[Tuple.Size[Subtypes]]),0)

  inline def summonMembers0[Subtypes <: Tuple, Labels <: Tuple](arr: Array[Member], index: Int = 0): Array[(String,DeriveSchema[_])] =
    inline erasedValue[(Subtypes,Labels)] match
      case (_: EmptyTuple,_: EmptyTuple) => arr
      case (_: (s *: stail), _: (l *: ltail)) =>
        summonFrom {
          case s: Schema[`s`] => 
            arr(index) = (constValue[l].asInstanceOf[String],instance(s))
            summonMembers0[stail,ltail](arr,index + 1)
          case _ =>
            arr(index) = (constValue[l].asInstanceOf[String],summonInline[DeriveSchema[s]])
            summonMembers0[stail,ltail](arr, index + 1)
        }

end DeriveSchema
