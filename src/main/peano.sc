trait Nat
case object Z extends Nat
case class S[N <: Nat](n: N) extends Nat

type One = S[Z.type]
type Two = S[One]
type Three = S[Two]
type Four = S[Three]
type Five = S[Four]
type Six = S[Five]
type Seven = S[Six]

def inc[N <: Nat](n: N): S[N] = S(n)

val _0: Z.type = Z
val _1 = S(_0)
val _2 = S(_1)
val _3 = S(_2)
val _4 = S(_3)
val _5 = S(_4)
val _6 = S(_5)


inc(_0)

trait Plus[A <: Nat, B <: Nat] {
  type Out <: Nat
}

object Plus {
  def apply[A <: Nat, B <: Nat, O <: Nat]: Aux[A, B, O] = new Plus[A, B] {
    type Out = O
  }

  type Aux[A <: Nat, B <: Nat, O <: Nat] = Plus[A, B] {type Out = O}


}

implicit def zeroPlusAny[N <: Nat]: Plus.Aux[N, Z.type, N] = Plus[N, Z.type, N] // n + 0
implicit def aPlusAny[A <: Nat, B <: Nat, P <: Nat](implicit plus: Plus.Aux[S[A], B, P]): Plus.Aux[A, S[B], P] = Plus[A, S[B], P]

def plus[A <: Nat, B <: Nat, PO <: Nat](a: A, b: B)(implicit plus: Plus.Aux[A, B, PO]): PO = null.asInstanceOf[PO]

plus(_1, _1)

/*
(A$A902.this.aPlusAny[dot.S[dot.S[dot.Z.type]], dot.S[dot.S[dot.Z.type]], dot.S[dot.S[dot.S[dot.S[dot.S[dot.Z.type]]]]]](
  A$A902.this.aPlusAny[dot.S[dot.S[dot.S[dot.Z.type]]], dot.S[dot.Z.type], dot.S[dot.S[dot.S[dot.S[dot.S[dot.Z.type]]]]]](
    A$A902.this.aPlusAny[dot.S[dot.S[dot.S[dot.S[dot.Z.type]]]], dot.Z.type, dot.S[dot.S[dot.S[dot.S[dot.S[dot.Z.type]]]]]](
      A$A902.this.zeroPlusAny[dot.S[dot.S[dot.S[dot.S[dot.S[dot.Z.type]]]]]]))))
*/

plus(_2, _3) // res1: Five = null

trait ToInt[N <: Nat]{
  def toInt:Int
}

implicit def zero:ToInt[Z.type] = new ToInt[Z.type] {
  override def toInt: Int = 0
}

implicit def s[N <: Nat, SN <: S[N]](implicit inter:ToInt[N]):ToInt[S[N]] = new ToInt[S[N]] {
  override def toInt: Int = 1 + inter.toInt
}

def deanon[N <: Nat](implicit toint:ToInt[N]):Int = toint.toInt


case class Plused[A <: Nat, B <: Nat]() {
  def toInt[P <: Nat](implicit plus:Plus.Aux[A,B,P], inter:ToInt[P]):Int = inter.toInt
}

Plused[Four,Three]().toInt


def cmp[A <: Nat, B <: Nat](a:A, b:B)(implicit ev: A =:= B):Boolean = true



cmp(plus(_2, _3), _5)


