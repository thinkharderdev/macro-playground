package dev.thinkharder

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import scala.quoted.*

object generic extends SchemaMacros

trait SchemaMacros:
  inline def getFields[Labels <: Tuple, Params <: Tuple]: List[Field[_]] =
    inline erasedValue[(Labels,Params)] match
      case _: (EmptyTuple,EmptyTuple) => Nil
      case _: (l *: ltail, p *: ptail) =>
        summonFrom {
          case m: Mirror.SumOf[`p`] =>
            Field(constValue[l].asInstanceOf[String], Lazy(() => derivedMirrorSum(m))) :: getFields[ltail,ptail]
          case m: Mirror.ProductOf[`p`] =>
            Field(constValue[l].asInstanceOf[String], Lazy(() => derivedMirrorProduct(m)))  :: getFields[ltail,ptail]
          case _ =>
            Field(constValue[l].asInstanceOf[String], summonInline[Schema[`p`]])  :: getFields[ltail,ptail]
        }


  inline def deriveCases[A, SubtypeTuple <: Tuple](m: Mirror.SumOf[A]): List[Case[_,A]] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple => Nil
      case _: (s *: tail) =>
        Case[s,A]("case", summonInline[Schema[s]]) :: deriveCases[A,tail](m)

  inline def derivedMirrorProduct[A](inline product: Mirror.ProductOf[A]): Schema[A] =
    inline erasedValue[(product.MirroredElemLabels, product.MirroredElemTypes)] match
      case _: (l *: ltail,p *: ptail) =>
        Record(getFields[l *: ltail, p *: ptail])

  transparent inline def derivedMirrorSum[A](inline sum: Mirror.SumOf[A]): Schema[A] =
    inline erasedValue[sum.MirroredElemTypes] match
      case _: EmptyTuple => EnumN(Nil)
      case _: (s *: EmptyTuple) =>
        Enum1(Case[s,A]("case1", summonInline[Schema[s]]))
      case _: (s1 *: s2 *: EmptyTuple) =>
        Enum2(
          Case[s1,A]("case1", summonInline[Schema[s1]]),
          Case[s2,A]("case2", summonInline[Schema[s2]])
        )
      case _: (s1 *: s2 *: s3 *: EmptyTuple) =>
        Enum3(
          Case[s1,A]("case1", summonInline[Schema[s1]]),
          Case[s2,A]("case2", summonInline[Schema[s2]]),
          Case[s3,A]("case2", summonInline[Schema[s3]])
        )
      case _: (s1 *: tail) =>
        EnumN(deriveCases[A,s1 *: tail](sum))


  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Schema[A] = inline mirror match
    case sum: Mirror.SumOf[A] => Lazy(() => derivedMirrorSum(sum))
    case product: Mirror.ProductOf[A] =>
      Lazy(() => derivedMirrorProduct(product))

  inline given derived[A](using Mirror.Of[A]): Schema[A] =
    summonFrom {
      case s: Schema[A] => s
      case _ => derivedMirror[A]
    }
end SchemaMacros
