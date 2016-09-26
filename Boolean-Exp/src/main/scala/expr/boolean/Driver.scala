package expr.boolean

import sexpr._
import BooleanExpr._

object Driver extends App {

  val e1 = ifs(&&(F,T), ||(T,F), &&(F,F))

  val b1 = parse(e1)
  println(b1)
  val r1 = eval(b1)
  println(r1)

}