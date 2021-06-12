package dev.thinkharder

sealed trait Schema[A]: 
  self =>
  def <*>[B](that: Schema[B]): Schema[(A, B)] = self.zip(that)
  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.TupleSchema(self, that)
  def map[B](f: A => B): Schema[B] =
    Schema.Transform[A, B](self, a => f(a))

object Schema:
  object StringSchema extends Schema[String]
  
  final case class Optional[A](schema: Schema[A]) extends Schema[Option[A]]

  final case class ListSchema[A](schema: Schema[A]) extends Schema[List[A]]

  given Schema[String] = StringSchema

  given [A] (using s: Schema[A]): Schema[Option[A]] = Optional(s)

  given [A] (using s: Schema[A]): Schema[List[A]] = ListSchema(s)

  def apply[A](using s: Schema[A]): Schema[A] = s

  def defer[A](schema: => Schema[A]): Schema[A] = Lazy(() => schema)

  final case class TupleSchema[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]

  final case class CaseObject[Z](instance: Z) extends Schema[Z]

  final case class Case[A, Z](id: String, schema: Schema[A])

  final case class Enum1[A, Z](case1: Case[A, Z])                                extends Schema[Z]
  final case class Enum2[A1, A2, Z](case1: Case[A1, Z], case2: Case[A2, Z]) extends Schema[Z]
  final case class Enum3[A1, A2, A3, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z])
    extends Schema[Z]
  final case class EnumN[Z](cases: List[Case[_, Z]]) extends Schema[Z]

  final case class Field[A](label: String, schema: Schema[A])

  final case class Record[A](fields: List[Field[_]]) extends Schema[A]

  final case class Transform[A, B](schema: Schema[A], f: A => B)
    extends Schema[B]

  final case class Lazy[A](private val schema0: () => Schema[A]) extends Schema[A]: 
    lazy val schema: Schema[A] = schema0()
    override def toString: String = s"Lazy($schema)"
end Schema
