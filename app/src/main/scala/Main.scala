package dev.thinkharder

import dev.thinkharder.model.*
import zio.test.magnolia.DeriveGen.derived
import zio.test.magnolia.DeriveGen
import DeriveSchema.derived

@main def hello: Unit =
  // val rec1 = Rec("foo",None)
  // val rec2 = Rec("bar",None)
  // println(Rec.compare(rec1,rec2))
  // println(Rec.compare(rec1,rec1))

  println(DeriveSchema[Rec])
  // println(DeriveSchema[Teacher])
  // println(DeriveGen[Rec])