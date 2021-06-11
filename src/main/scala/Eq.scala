package dev.thinkharder

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

inline def summonAll[T <: Tuple]: List[Eq[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Eq[t]] :: summonAll[ts]

trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:

  def apply[A](using eq: Eq[A]): Eq[A] = eq

  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y

  given Eq[String] with
    def eqv(x: String, y: String) = x == y

  given [A] (using e: Eq[A]): Eq[Option[A]] =
    new Eq[Option[A]]:
      def eqv(x: Option[A], y: Option[A]): Boolean =
        (x,y) match
          case (Some(x1), Some(y1)) => e.eqv(x1,y1)
          case (None,None) => true
          case _ => false

  def check(elem: Eq[_])(x: Any, y: Any): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        iterator(x).zip(iterator(y)).zip(elems.iterator).forall {
          case ((x, y), elem) => check(elem)(x, y)
        }

  inline given derived[T](using m: Mirror.Of[T]): Eq[T] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
end Eq