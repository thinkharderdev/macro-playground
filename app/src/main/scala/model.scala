package dev.thinkharder


object model:
  import DeriveSchema.derived

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  case class Rec(value: String, otherValue: Option[String])

  object Rec:
    def compare(l: Rec, r: Rec)(using e: Eq[Rec]): Boolean = e.eqv(l,r)
    // def schema: Schema[Rec] = Schema[Rec]

  val rec1 = Rec("foo",None)
  val rec2 = Rec("foo",None)

  case class Simple(value: Int) 

  object Simple:
    def compare(l: Simple, r: Simple)(using e: Eq[Simple]): Boolean = e.eqv(l,r)

  val s1 = Simple(0)
  val s2 = Simple(0)

  case class Teacher(students: List[String])
  case class Student(teacher: Teacher)