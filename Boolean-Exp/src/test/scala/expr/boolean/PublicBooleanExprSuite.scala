package expr.boolean

import org.scalatest.FunSuite
import expr.boolean.BooleanExpr._
import sexpr._

class PublicBooleanExprSuite extends FunSuite {

  test("[2] Boolean language T must parse to True") {
    val r = parse(T)
    assert(r.toString == "True")
  }

  test("[2] Boolean language F must parse to False") {
    val r = parse(F)
    assert(r.toString == "False")
  }

  test("[3] Boolean language &&(T,F) must parse to And(True, False)") {
    val r = parse(&&(T,F))
    println(r)
    assert(r.toString == "And(True,False)")
  }

  test("[3] Boolean language ||(T,F) must parse to Or(True, False)") {
    val r = parse(||(T,F))
    assert(r.toString == "Or(True,False)")
  }

  test("[4] Boolean language T must evaluate to True") {
    val r = eval(parse(T))
    assert(r.toString == "True")
  }

  test("[4] Boolean language F must evaluate to False") {
    val r = eval(parse(F))
    assert(r.toString == "False")
  }

  test("[5] Boolean language &&(T,F) must evaluate to False") {
    val r = eval(parse(&&(T,F)))
    assert(r.toString == "False")
  }

  test("[5] Boolean language ||(T,F) must evaluate to True") {
    val r = eval(parse(||(T,F)))
    assert(r.toString == "True")
  }

  test("[6] Boolean language &&(||(T,F),&&(F,T)) must evaluate to False") {
    val r = eval(parse(&&(||(T,F),&&(F,T))))
    assert(r.toString == "False")
  }

  test("[6] Boolean language ||(&&(T,T),||(F,T)) must evaluate to True") {
    val r = eval(parse(||(&&(T,T),||(F,T))))
    assert(r.toString == "True")
  }

  test("[6] Boolean language not(&&(F,T)) must evaluate to True") {
    val r = eval(parse(not(&&(F,T))))
    assert(r.toString == "True")
  }

}
