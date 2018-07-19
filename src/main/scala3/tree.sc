sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
  def size[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def max(t:Tree[Int]):Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => max(l) max max(r)
  }

  def depth[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A,B](t:Tree[A])(f: A => B):Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t:Tree[A])(f: A => B)(combine: (B,B) => B):B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => combine(fold(l)(f)(combine),fold(r)(f)(combine))
  }

  def mapViaFold[A,B](t:Tree[A])(f: A => B):Tree[B] = fold(t) {x => Leaf(f(x)):Tree[B] } { (l,r) => Branch(l,r)}


}

/****
  *            *
  *           / \
  *          1   *
  *             / \
  *            2   *
  *               / \
  *              4   5
  */
val tree:Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(4), Leaf(5))))


Tree.size(tree)
Tree.fold(tree)(_ => 1)(_ + _)

Tree.max(tree)
Tree.fold(tree)((x:Int) => x)( (x:Int,y:Int) => x max y)

Tree.depth(tree)
Tree.fold(tree)(_ => 1)( (x:Int,y:Int) => (x+1) max (y+1))

Tree.map(tree)(_ * 2)
Tree.mapViaFold(tree)(_ *  2)
