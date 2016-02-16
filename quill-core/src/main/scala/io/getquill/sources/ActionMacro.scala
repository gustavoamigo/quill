package io.getquill.sources

import scala.reflect.macros.whitebox.Context

import io.getquill._
import io.getquill.ast._
import io.getquill.util.Messages.fail

import io.getquill.ast.AstShow.astShow
import io.getquill.util.Show.Shower

trait ActionMacro extends EncodingMacro {
  this: SourceMacro =>

  val c: Context
  import c.universe.{ Ident => _, _ }

  def runAction[R, S, T](ast: Ast, params: List[(Ident, Type)])(implicit r: WeakTypeTag[R], s: WeakTypeTag[S], t: WeakTypeTag[T]): Tree = {
    val (action, returningValues) = ExtractReturning(ast)

    returningValues match {
      case None      => runActionNoReturning[S, T](action, params)
      case Some(ast) => runActionReturning[R, S, T](action, params, ast)
    }
  }

  private def runActionReturning[R, S, T](action: Ast, params: List[(Ident, Type)], property: Ast)(implicit r: WeakTypeTag[R], s: WeakTypeTag[S], t: WeakTypeTag[T]): Tree = {

    c.echo(c.enclosingPosition, s"debug runActionReturning action=${action.show} property=$property")

    val returningValues = encoding[T](property, Encoding.inferDecoder[R](c))
    val extractor = selectResultExtractor[R](returningValues)

    c.warning(c.enclosingPosition, s"extractor=$extractor")

    params match {
      case Nil =>
        val q = q"${c.prefix}.execute(${prepare(action, params.map(_._1))}._1, $extractor)"
        c.warning(c.enclosingPosition, s"q=$q")
        q

      case List((param, tpe)) if (t.tpe.erasure <:< c.weakTypeOf[UnassignedAction[Any]].erasure) =>
        val encodingValue = encoding(param, Encoding.inferEncoder[S](c))(c.WeakTypeTag(tpe))
        val bindings = bindingMap(encodingValue)
        val idents = bindings.map(_._1).toList
        val assignedAction = AssignedAction(action, idents.map(k => Assignment(Ident("x"), k.name, k)))
        val encodedParams = EncodeParams.raw[S](c)(bindings.toMap)
        val q = expandedTreeReturning(assignedAction, idents.toList, List(tpe), encodedParams, extractor)
        c.warning(c.enclosingPosition, s"q=$q")
        q

      case params =>
        val encodedParams = EncodeParams[S](c)(bindingMap(params))
        val q = expandedTreeReturning(action, params.map(_._1), params.map(_._2), encodedParams, extractor)
        c.warning(c.enclosingPosition, s"q=$q")
        q
    }
  }

  private def expandedTreeReturning(action: Ast, idents: List[Ident], paramsTypes: List[Type], encodedParams: Tree, extractor: Tree) =
    q"""
  {
    val (sql, bindings: List[io.getquill.ast.Ident]) =
      ${prepare(action, idents)}

    ${c.prefix}.execute[(..$paramsTypes)](
      sql,
      value => $encodedParams(bindings.map(_.name))
      )
  }
  """

  private def runActionNoReturning[S, T](action: Ast, params: List[(Ident, Type)])(implicit s: WeakTypeTag[S], t: WeakTypeTag[T]): Tree =
    params match {
      case Nil =>
        q"${c.prefix}.execute(${prepare(action, params.map(_._1))}._1)"

      case List((param, tpe)) if (t.tpe.erasure <:< c.weakTypeOf[UnassignedAction[Any]].erasure) =>
        val encodingValue = encoding(param, Encoding.inferEncoder[S](c))(c.WeakTypeTag(tpe))
        val bindings = bindingMap(encodingValue)
        val idents = bindings.map(_._1).toList
        val assignedAction = AssignedAction(action, idents.map(k => Assignment(Ident("x"), k.name, k)))
        val encodedParams = EncodeParams.raw[S](c)(bindings.toMap)
        expandedTree(assignedAction, idents.toList, List(tpe), encodedParams)

      case params =>
        val encodedParams = EncodeParams[S](c)(bindingMap(params))
        expandedTree(action, params.map(_._1), params.map(_._2), encodedParams)
    }

  private def expandedTree(action: Ast, idents: List[Ident], paramsTypes: List[Type], encodedParams: Tree) =
    q"""
    {
      val (sql, bindings: List[io.getquill.ast.Ident]) =
        ${prepare(action, idents)}

      ${c.prefix}.execute[(..$paramsTypes)](
        sql,
        value => $encodedParams(bindings.map(_.name)))
    }
    """

  private def bindingMap(value: Value, option: Boolean = false): List[(Ident, (Tree, Tree))] =
    value match {
      case CaseClassValue(tpe, values) =>
        values.flatten.map(bindingMap(_)).flatten
      case OptionValue(value) =>
        bindingMap(value, option = true)
      case SimpleValue(ast, encoder, optionEncoder) =>
        val (ident, tree) = bindingTree(ast)
        val enc = if (option) optionEncoder else encoder
        List((ident, (enc, tree)))
    }

  private def bindingTree(ast: Ast): (Ident, Tree) =
    ast match {
      case Property(ast: Property, name) =>
        val (ident, tree) = bindingTree(ast)
        (ident, q"$tree.${TermName(name)}")
      case Property(ast: Ident, name) =>
        (Ident(name), q"value.${TermName(name)}")
      case other =>
        fail(s"Can't bind '$other'.")
    }

  private def bindingMap(params: List[(Ident, Type)]): collection.Map[Ident, (Type, Tree)] =
    params match {
      case (param, tpe) :: Nil =>
        collection.Map((param, (tpe, q"value")))
      case params =>
        (for (((param, tpe), index) <- params.zipWithIndex) yield {
          (param, (tpe, q"value.${TermName(s"_${index + 1}")}"))
        }).toMap
    }
}
