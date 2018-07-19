package object my {
  import scala.reflect.macros.whitebox
  import scala.language.experimental.macros

  def id(x:Int):Int = macro IdImpl.impl

  def debug[A](e:A):A = macro Debug.impl[A]

  object IdImpl {
    def impl(c:whitebox.Context)(x:c.Expr[Int]):c.Expr[Int] = {
      x
    }
  }

  object Debug {
    def impl[A : c.WeakTypeTag](c:whitebox.Context)(e:c.Expr[A]):c.Expr[A] = {
      import c.universe._
      val body = q"""val __start = System.currentTimeMillis(); val t = $e; println("Expression " + ${e.tree.toString()} + " reduced to " + t + " in " + (System.currentTimeMillis() - __start) + "ms"); t"""
      c.Expr[A](body)
    }
  }
}
