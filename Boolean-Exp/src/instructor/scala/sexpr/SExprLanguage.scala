package sexpr

/**
 * This trait defines the s-expression language.
 *
 * The s-expression language is very simple. An s-expression (sexpr)
 * is either a "symbol" or a list of sexprs. A sexpr in written form
 * is typically expressed as:
 *
 * (x y z)
 * (x y (a b c) d (e))
 *
 * We define sexprs here using case classes - in the same spirit as
 * the List algebraic data type. We also provide some useful functions
 * for creating sexprs. So, the above examples would be expressed
 * using our implementation like so:
 *
 * l(s("x"), s("y"), s("z"))
 * l(s("x"), s("y"), l(s("a"), s("b"), s("c")), s("d"), l(s("e")))
 *
 * You should go to the scala console and play around with these
 * types to get a better understanding of how they work.
 *
 * scala> object SL extends sexpr.SExprLanguage
 * scala> import SL._
 * scala> l(s("x"), s("y"), s("z"))
 */
trait SExprLanguage {

  // We use a sealed trait SExpr so we can't extend it outside of
  // the SExprLanguage trait.
  sealed trait SExpr

  // Represents a symbol.
  case class SSymbol(name: String) extends SExpr

  // Represents an abstract sexpr list.
  trait SList extends SExpr

  // Represents the end of a sexpr list.
  case object SNil extends SList

  // Represents the list.
  case class SCons(head: SExpr, tail: SList) extends SList

  // A helper function to create sexpr symbols easily.
  def s(n: String) = SSymbol(n)

  // A helper function to create sexpr lists easily.
  def l(xs: SExpr*): SList =
    if (xs.isEmpty) SNil
    else SCons(xs.head, l(xs.tail: _*))
}
