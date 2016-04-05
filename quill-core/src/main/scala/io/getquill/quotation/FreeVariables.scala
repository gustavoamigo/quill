package io.getquill.quotation

import io.getquill.ast._

import scala.reflect.macros.whitebox.Context

case class FreeVariables[T](state: List[CompileTimeBinding[T]])
  extends StatefulTransformer[List[CompileTimeBinding[T]]] {

  override def apply(ast: Ast): (Ast, StatefulTransformer[List[CompileTimeBinding[T]]]) =
    ast match {
      case binding: CompileTimeBinding[T] => (binding, FreeVariables(binding :: state))
      case other =>
        super.apply(other)
    }
}

object FreeVariables {
  def extract[C <: Context](c: C)(ast: Ast): List[CompileTimeBinding[c.Tree]] =
    new FreeVariables[c.Tree](List.empty[CompileTimeBinding[c.Tree]])(ast) match {
      case (_, transformer) =>
        transformer.state
    }
}
