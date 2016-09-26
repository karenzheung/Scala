package expr.boolean

import sexpr._

/**
 * This object defines types for creating and evaluating boolean
 * expressions. In particular, it should define the six different
 * boolean expressions that are based on those defined by the
 * boolean sexpr language - except, here they are actual case
 * classes rather than s-expression data types.
 *
 * You must define six case classes that extend Expr (see below):
 *
 * (1) True
 * (2) False
 * (3) Not(e)
 * (4) And(e1,e2)
 * (5) Or(e1,e2)
 * (6) If(c,e1,e2)
 *
 * Note, that case classes are automatically provided a toString
 * method that will resemble how they were constructed. For example,
 *
 * scala> True
 * "True"
 * scala> Not(True)
 * "Not(True)"
 *
 * We will use these strings to test your code - so you must ensure
 * that your case classes exactly resemble the 1-6 definitions above.
 *
 * You must also implement the `parse` and `eval` methods defined
 * below. These methods will parse an SExpr type into a Expr type and
 * evaluate an Expr to an Expr respectively.
 */
object BooleanExpr extends BooleanLanguage {
  sealed trait Expr

  // TODO: Define your case classes here.
//makes the boolean expression True
  case object True extends Expr
//makes boolean expression False
  case object False extends Expr
//makes boolean expression Not by taking in an expression as parameter
  case class Not(e1: Expr) extends Expr
  //makes boolean expression And by taking in 2 expressions as parameters
  case class And(e1: Expr, e2: Expr) extends Expr
  //makes boolean expression Or by taking in 2 expressions as parameters

  case class Or(e1: Expr, e2: Expr) extends Expr
  //makes boolean expression If by taking in 3 expressions as parameters
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr





  // TODO: implement the parse method.
  // The parse method translates an SExpr type into the Expr types
  // that you defined above. This allows us to define boolean
  // expressions using sexpr form - which decouples your
  // implementation from definitions of boolean expression.
  //
  // Example:
  //
  // scala> parse(&&(T,F))
  // And(True,False)
  //
  // You will need to pay attention to the structure of the sexprs
  // for proper translation.
  //
  // You must you pattern matching and recursion in your
  // implementation.
  //
  // If you are given an improperly formatted boolean sexpr you must
  // throw an IllegalArgumentException.

  /**
   * Parses the SExpr by matching against the ss
   * and recursively calling parse on the nested SExpr
   * until the base case (True or false) is reaching
   *
   * @throws IllegalArgumentException if the given SExpr is not
   * one of the six valid boolean expression
   * @param ss  the SExpr to parse
   * @return an Expr that represents a boolean expression
   */
  def parse(ss: SExpr): Expr = ss match{
    case T => True
    case F => False
    case SCons(AND, SCons(e1, SCons(e2, SNil))) => And(parse(e1), parse(e2))
    case SCons(NOT, SCons(e1, SNil)) => Not(parse(e1))
    case SCons(OR, SCons(e1, SCons(e2, SNil))) =>  Or(parse(e1), parse(e2))
    case SCons(IF,SCons(e1, SCons(e2, SCons(e3,SNil)))) =>  If(parse(e1), parse(e2), parse(e3))
    case _ => throw new IllegalArgumentException
  }

  // TODO: implement the eval method.
  //
  // The eval function "evaluates" a boolean expression. Your function
  // must implement the following rules:
  //
  // (1)  true                  => true
  // (2)  false                 => false
  // (3)  !true                 => false
  // (4)  !false                => true
  // (5)  true && e             => e
  // (6)  false && e            => false
  // (7)  true || e             => true
  // (8)  false || e            => e
  // (9)  if(true) e1 else e2   => e1
  // (10) if(false) e1 else e2  => e2
  //
  // Here are some things you need to consider:
  //
  // (1) When you see an expression e on the right-hand side of a
  //     rule you will need to recursively evaluate e.
  //
  // (2) Although we have given you most rules above, there are other
  //     fules that you will need to implement for proper evaluation.
  //     That is, you must adhere to (4) below for your implementation
  //     to be correct.
  //
  // (3) You need to consider commutativity of operators.
  //
  // (4) Your eval function will always evaluate the given expression
  //     to either the true or false boolean expression.
  //

  /**
   * Evaluates the boolean expression by recursively calling eval on itself
   *
   *  For more complex expressions like And, Or, and If
   *  recursively calls eval on the nested expressions until
   *  reaches the base case True or False
   *
   * Each expression must return either a true or false
   *
   * @param e  the line to split
   * @return an expression
   */
  def eval(e: Expr): Expr = e match{
    case True => True
    case False => False
    case Not(True) => False
    case Not(False) => True
    case And(True, e) => eval(e)
    case And(False, e) => False
    case Or(True, e) => True
    case Or(False, e) => eval(e)
    case If(True, e1, e2) => eval(e1)
    case If(False, e1, e2) => eval(e2)
    case And(e1, e2) => eval(And(eval(e1), e2))        //the cases down here are needed because each Expr
    case Or(e1, e2) => eval(Or(eval(e1), e2))          // could be a nested Expr such as If(And(e1...))
    case Not(e1) => eval(Not(eval(e1)))                //more general case that leads back to the 10 more specific cases
    case If(e1, e2, e3) => eval(If(eval(e1), e2, e3))  //only need to eval on first expression because the If(True,..)
                                                       // If(False,..) takes care of the other cases
  }
}
