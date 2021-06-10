import dev.thinkharder._

sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

case class Rec(value: String, otherValue: String)

@main def hello: Unit =
  println(DeriveSchema[Tree[String]])
  println(DeriveSchema[Rec])