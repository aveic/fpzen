package object dot {

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
}