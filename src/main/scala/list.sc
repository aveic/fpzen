sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(h,t) => h * product(t)
  }

  def apply[A](as:A*):List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](l:List[A]) = l match {
    case Nil => throw new Exception("Calling tail on an empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l:List[A], nh:A):List[A] = l match {
    case Nil => Cons(nh, Nil)
    case Cons(_, t) => Cons(nh, t)
  }

  def drop[A](l: List[A], n:Int):List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => if (n > 1) drop(t, n - 1) else t
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Calling init on a empty list")
    case Cons(_,Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match{
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l:List[A]):Int = foldRight(l, 0) { (_,acc) => acc + 1  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l:List[A]):List[A] = foldLeft(l, Nil:List[A])( (acc,el) => Cons(el,acc))

  def append[A](l1:List[A], l2:List[A]):List[A] = List.foldRight(l1,l2){ (el,acc) => Cons(el, acc)}

  def flatten[A](ls:List[List[A]]):List[A] = List.foldRight(ls, Nil:List[A])(append)

  def map[A,B](l:List[A])(f: A => B):List[B] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(f(h), List.map(t)(f))
  }



  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter[A](l:List[A])(f: A => Boolean):List[A] = flatMap(l)( x => if (f(x)) Cons(x,Nil) else Nil)

  def exists[A](l:List[A])(f: A => Boolean):Boolean = l match {
    case Nil => false
    case Cons(h,t) => if (f(h)) true else exists(t)(f)
  }

  def zipWithIndex[A](l:List[A]):List[(Int,A)] = {
    val sz = length(l)
    foldRight(l, (0,Nil:List[(Int,A)]))( (el, acc) => {
      val (n, accL) = acc
      (n+1, Cons((sz - n - 1, el), accL))
    })._2
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = exists(allTails(sup))( t => isPrefixOf(sub, t))

  def isPrefixOf[A](needle:List[A], haystack:List[A]):Boolean = (needle, haystack) match {
    case (Nil, _) => true
    case (Cons(nh, nt), Cons(hh, ht)) if nh == hh => isPrefixOf(nt, ht)
    case _ => false
  }

  def allTails[A](l:List[A]):List[List[A]] = {
    @annotation.tailrec
    def go(tail: List[A], acc:List[List[A]]):List[List[A]] = tail match {
      case Nil => acc
      case Cons(h, t) => go(t, Cons(t, acc))
    }

    go(l, Nil)
  }
}


List.drop(List(1,2,3,4), 3)

List.init(List(1,2,3,4))

val l = List(1,2,3,4)
val l2 = List(5,6,7,8)
List.length(l)

List.append(l,l2)

List.flatten(List(l,l2,l))

List.filter(l)(_ % 2 == 0)

List.hasSubsequence(List(1,2,3,4), List(1))
List.hasSubsequence(List(1,2,3,4), List(2))
List.hasSubsequence(List(1,2,3,4), List(1,2))
List.hasSubsequence(List(1,2,3,4), List(2,3,4))
List.hasSubsequence(List(1,2,3,4), List(4))
List.hasSubsequence(List(1,2,3,4), List(1,2,4))
List.hasSubsequence(List(1,2,3,4,2,3,1,2,4,5), List(1,2,4))

List.zipWithIndex(List("a", "b", "c"))