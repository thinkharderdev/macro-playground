package dev.thinkharder

import dev.thinkharder.model.*

@main def hello: Unit =
//  println(DeriveSchema[Tree[String]])
  // val recEq = Eq[Rec]
  println(Rec.schema)
  // println(Rec.compare(rec1,rec2))
  // println(Simple.compare(s1,s2))
  // println(Schema[Teacher])
  // println(Schema[Student])