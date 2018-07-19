import scala.annotation.implicitNotFound
import scala.languageFeature.implicitConversions

sealed trait HHList

case class HHCons[A, B <: HHList](head: A, tail: B) extends HHList {
  def ::[X](x:X): X :: A :: B = HHCons(x, this)
  override def toString: String = s"$head :: $tail"
}

trait HHNil extends HHList
case object HHNil extends HHNil {
  def ::[B](b:B): B :: HHNil = HHCons(b, HHNil)
}

type ::[A, B <: HHList] = HHCons[A, B]

val x: Int :: String :: HHNil = HHCons(2, HHCons("abc", HHNil))

val x2: Int :: String :: HHNil = 2 :: "abc" :: HHNil // which is actually: (HHNil :: "abc") :: 2



trait CsvEncoder[A] {
  def encode(value:A):String
}

def buildEncoder[A](f: A => String):CsvEncoder[A] = new CsvEncoder[A] {
  override def encode(value: A): String =  f(value)
}

// traversal
implicit val intIncoder:CsvEncoder[Int] = buildEncoder(_.toString)
implicit val strIncoder:CsvEncoder[String] = buildEncoder(identity)
implicit val boolEncoder:CsvEncoder[Boolean] = buildEncoder(if(_) "yes" else "no")
implicit val hhnilEncoder:CsvEncoder[HHNil] = buildEncoder(_ => "END")

implicit def hhlistEncoder[H, T <: HHList](
                                          implicit headEncoder:CsvEncoder[H],
                                          tailEncoder:CsvEncoder[T]
                                          ): CsvEncoder[H :: T] = new CsvEncoder[H :: T] {
  override def encode(value: H :: T): String = {
    val encodedHead = headEncoder.encode(value.head)
    val encodedTail = tailEncoder.encode(value.tail)

    s"$encodedHead,$encodedTail"
  }
}

// summoner method
def encode[H, T <: HHList](hlist: H :: T)(implicit encoder: CsvEncoder[H :: T]):String = encoder.encode(hlist)

encode(1 :: 2 :: 3 :: 4 :: HHNil)

// that's good ok what's next: ops classes


// how do we get it from any case class



// ok we good, 1..22
implicit def tuple1[A](t1: Tuple1[A]): A :: HHNil = t1._1 :: HHNil
implicit def tuple2[A,B](t2: (A,B)): A :: B :: HHNil = t2._1 :: t2._2 :: HHNil
implicit def tuple3[A,B,C](t3: (A,B,C)): A :: B :: C :: HHNil = t3._1 :: t3._2 :: t3._3 :: HHNil


encode((2, "abc"))

case class X(i:Int, s:String)

val xx = X(1,"abc")

encode(X.unapply(xx).get) // we can get those with macro

// hlist ops

trait IsHHCons[L <: HHList] {
  type H
  type T <: HHList

  def head(l : L) : H
  def tail(l : L) : T
  def cons(h : H, t : T) : L
}

object IsHHCons {
  type Aux[L <: HHList, HO, TO <: HHList] = IsHHCons[L] { type H = HO; type T = TO }
  def apply[L <: HHList](implicit isHHCons: IsHHCons[L]): Aux[L, isHHCons.H, isHHCons.T] = isHHCons

  implicit def isHHCons[HO, TO <: HHList]:Aux[HO :: TO, HO,TO] = new IsHHCons[HO :: TO] {
    type H = HO
    type T = TO

    def head(l: HO :: TO) = l.head
    def tail(l: HO :: TO) = l.tail
    def cons(h: HO, t: TO): HO :: TO = HHCons(h, t)
  }
}


trait Second[L <: HHList] {
  type Out
  def apply(value:L):Out
}

object Second {
  type Aux[L <: HHList, O] = Second[L] { type Out = O }
  def apply[L <: HHList](implicit snd: Second[L]): Aux[L, snd.Out] = snd // need for types
  implicit def second[L <: HHList, HO, TO <: HHList, H2O, T2O <: HHList](
                                                  implicit isHCons: IsHHCons.Aux[L, HO, TO],
                                                  isHCons2: IsHHCons.Aux[TO, H2O, T2O]
                                                ): Aux[L, H2O] = new Second[L] {
    type Out = H2O
    def apply(value: L): H2O = isHCons2.head(isHCons.tail(value))
  }
}



def second[T <: HHList](hlist: T)(implicit snd: Second[T]):snd.Out = snd.apply(hlist)


second(x)