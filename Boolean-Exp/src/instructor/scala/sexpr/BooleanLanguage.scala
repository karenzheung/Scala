package sexpr

/**
 * This trait defines the boolean s-expression language.
 *
 * It builds off of the base sexpr language (SExprLanguage) with
 * specific values and functions for creating boolean sexprs.
 *
 * Our boolean sexpr language is simple:
 *
 * (1) T represents true
 * (2) F represents false
 * (3) AND represents boolean &&
 * (4) OR represents boolean ||
 * (5) IF represents a conditional if expression
 * (6) NOT represents boolean !
 *
 * We also define helper functions for building boolean exprs.
 *
 * (1) not(e) represents boolean !e
 * (2) &&(e1,e2) represents boolean e1 && e2
 * (3) ||(e1,e2) represents boolean e1 || e2
 * (4) ifs(c,e1,e2) represents conditional if expression if(c,e1,e2)
 *
 * Thus, the express if(!true) true && false else true would be
 * expressed in our language defined here as:
 *
 *   ifs(not(T),&&(T,F),T)
 *
 * You should play aroud with this language in the scala console to
 * get a better understanding on how these types work:
 *
 * scala> object BL extends sexpr.BooleanLanguage
 * scala> import BL._
 * scala> ifs(not(T),&&(T,F),T)
 */
trait BooleanLanguage extends SExprLanguage {
  val T   = s("true")   // A symbol to represent true
  val F   = s("false")  // A symbol to represent false
  val AND = s("&&")     // A symbol to represent &&
  val OR  = s("||")     // A symbol to represent ||
  val IF  = s("if")     // A symbol to represent if
  val NOT = s("!")      // A symbol to represent !

  // A helper function to create a boolean ! expression.
  def not(e: SExpr): SExpr = l(NOT, e)

  // A helper function to create a boolean && expression.
  def &&(e1: SExpr, e2: SExpr): SExpr = l(AND, e1, e2)

  // A helper function to create a boolean || expression.
  def ||(e1: SExpr, e2: SExpr): SExpr = l(OR, e1, e2)

  // A helper function to create a boolean if expression.
  def ifs(c: SExpr, e1: SExpr, e2: SExpr): SExpr = l(IF, c, e1, e2)
}