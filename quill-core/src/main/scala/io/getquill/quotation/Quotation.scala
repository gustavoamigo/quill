package io.getquill.quotation

import io.getquill.util.Messages._

import scala.annotation.StaticAnnotation
import scala.reflect.ClassTag
import scala.reflect.macros.whitebox.Context
import io.getquill.ast._
import scala.reflect.internal.Symbols

import scala.collection.mutable

trait Quoted[+T] {
  def ast: Ast
}

case class QuotedAst(ast: Ast) extends StaticAnnotation

trait Quotation extends Liftables with Unliftables {

  val c: Context
  import c.universe._

  def quote[T: WeakTypeTag](body: Expr[T]) = {

    // dependent types for constructor parameter lists is not supported
    // see https://issues.scala-lang.org/browse/SI-5712
    val _freeVars = extractFreeVars(body.tree)
    val parser = new Parsing[c.type](c) {
      val freeVars = _freeVars
    }

    val ast = parser.astParser(body.tree)

    val bindings = FreeVariables.extract[c.type](c)(ast).map {
      case CompileTimeBinding(key, tree: Tree) =>
        val binding = TermName(s"binding_$key")
        q"def $binding = $tree"
    }

    val id = TermName(s"id${ast.hashCode}")

    q"""
      new ${c.weakTypeOf[Quoted[T]]} {
        @${c.weakTypeOf[QuotedAst]}($ast)
        def quoted = ast
        override def ast = $ast
        override def toString = ast.toString
        def $id() = ()
        ..$bindings
      }
    """
  }

  def doubleQuote[T: WeakTypeTag](body: Expr[Quoted[T]]) =
    body.tree match {
      case q"null" => c.fail("Can't quote null")
      case tree    => q"io.getquill.unquote($tree)"
    }

  def quotedFunctionBody(func: Expr[Any]) =
    func.tree match {
      case q"(..$p) => $b" => q"io.getquill.quote((..$p) => io.getquill.unquote($b))"
    }

  protected def unquote[T](tree: Tree)(implicit ct: ClassTag[T]) =
    astTree(tree).flatMap(astUnliftable.unapply).map {
      case ast: T => ast
    }

  private def astTree(tree: Tree) =
    for {
      method <- tree.tpe.decls.find(_.name.decodedName.toString == "quoted")
      annotation <- method.annotations.headOption
      astTree <- annotation.tree.children.lastOption
    } yield (astTree)

  private class FreeVarTraverser extends Traverser {
    val freeVars = mutable.LinkedHashSet[Symbol]()
    val declared = mutable.LinkedHashSet[Symbol]()
    def isLocalToBlock(sym: Symbol) = sym.owner.isTerm
    override def traverse(tree: Tree) = {
      tree match {
        case Function(args, _) =>
          args foreach { arg => declared += arg.symbol }
        case ValDef(_, _, _, _) =>
          declared += tree.symbol
        case _: Bind =>
          declared += tree.symbol
        case Ident(_) =>
          val sym = tree.symbol
          if ((sym != NoSymbol) && isLocalToBlock(sym) && sym.isTerm && !declared.contains(sym)) freeVars += sym
        case _ =>
      }
      super.traverse(tree)
    }
  }

  private def extractFreeVars(tree: Tree) = {
    val freeVarsTraverser = new FreeVarTraverser
    freeVarsTraverser.traverse(tree)
    freeVarsTraverser.freeVars.toList
  }

}
