package io.getquill.sources

import io.getquill.ast._

case class ExtractReturning(state: Option[Map])
  extends StatefulTransformer[Option[Map]] {

  override def apply(action: Action): (Action, StatefulTransformer[Option[Map]]) =
    action match {
      case Insert(Returning(a, b, c)) =>
        val (_, ct) = apply(c)
        (Insert(a), ExtractReturning(Some(Map(a, b, c))))
      case _ => super.apply(action)
    }
}

object ExtractReturning {
  def apply(ast: Ast): (Ast, Option[Map]) =
    new ExtractReturning(None)(ast) match {
      case (tast, transformer) =>
        (tast, transformer.state)
    }
}