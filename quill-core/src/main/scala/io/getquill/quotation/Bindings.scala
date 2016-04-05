package io.getquill.quotation

import io.getquill.ast.{ Ast, Binding, StatefulTransformer }

case class Bindings[T](state: List[Binding])
  extends StatefulTransformer[List[Binding]] {

  override def apply(ast: Ast): (Ast, StatefulTransformer[List[Binding]]) =
    ast match {
      case binding: Binding => (binding, Bindings(binding :: state))
      case other =>
        super.apply(other)
    }
}

object Bindings {
  def extract(ast: Ast): List[Binding] =
    new Bindings(List.empty[Binding])(ast) match {
      case (_, transformer) =>
        transformer.state
    }
}

