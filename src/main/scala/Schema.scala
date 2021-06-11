package dev.thinkharder

sealed trait Schema[A]

final case class CaseObject[Z](instance: Z) extends Schema[Z]

final case class Case[A, Z](id: String, schema: Schema[A])

final case class Enum1[A, Z](case1: Case[A, Z])                                extends Schema[Z]
final case class Enum2[A1, A2, Z](case1: Case[A1, Z], case2: Case[A2, Z]) extends Schema[Z]
final case class Enum3[A1, A2, A3, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z])
  extends Schema[Z]
final case class EnumN[Z](cases: Array[Case[_, Z]]) extends Schema[Z]

final case class Field[A](label: String, schema: Schema[A])

final case class Record[A](fields: Array[Field[_]]) extends Schema[A]

final class Lazy[A](private[this] val eval: () => Schema[A]) extends Schema[A]:
  lazy val value: Schema[A] = eval()
  override def toString: String = s"Lazy($value)"

object Lazy:
  def apply[A](a: => DeriveSchema[A]): Lazy[A] = new Lazy(() => a.derive)

object Schema:
  implicit object StringSchema extends Schema[String]
  
  final case class Optional[A](schema: Schema[A]) extends Schema[Option[A]]

  final case class Col[A](schema: Schema[A]) extends Schema[List[A]]

  given [A] (using s: Schema[A]): Schema[Option[A]] = Optional(s)

  given [A] (using s: Schema[A]): Schema[List[A]] = new Col(s)

  def apply[A](using s: Schema[A]): Schema[A] = s