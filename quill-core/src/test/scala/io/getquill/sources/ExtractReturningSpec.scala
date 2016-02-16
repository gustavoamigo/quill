package io.getquill.sources

import io.getquill._
import io.getquill.ast._

class ExtractReturningSpec extends Spec {
  "Extract returning" in {
    val ast = Function(List(Ident("x1")), Insert(Returning(Entity("TestEntity"), Ident("t"), Property(Ident("t"), "i"))))
    val (tast, returnings) = ExtractReturning(ast)
    returnings mustEqual Set(Ident("i"))
    tast mustEqual Function(List(Ident("x1")), Insert(Entity("TestEntity")))
  }
}