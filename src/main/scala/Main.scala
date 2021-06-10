import dev.thinkharder._
import dev.thinkharder.generic.derived

sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

case class Rec(value: String, otherValue: Rec)

//def schema =  Schema[Rec]

@main def hello: Unit =
  println(Schema[Rec])