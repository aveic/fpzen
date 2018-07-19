def fib(n:Int):Int = {
  @annotation.tailrec
  def loop(k:Int, prev1:Int, prev2: Int):Int = {
    if (k == n) prev1 + prev2
    else loop(k + 1, prev2, prev2 + prev1)
  }
  if (n < 3) 1 else loop(3, 1, 1)
}

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n:Int, isOrdered:Boolean):Boolean = {
    if (!isOrdered) false
    else
      if (n < as.length) loop(n + 1, ordered(as(n-1), as(n)))
      else isOrdered
  }

  if (as.length < 2) true else loop(1, true)
}

val ar1 = Array(1,2,3,4)
val ar2 = Array(1)
val ar3 = Array(1,2)
val ar4 = Array(1,2,3,5,1)
val ar5 = Array(2,1)

def cmp(a:Int,b:Int):Boolean = a <= b

isSorted(ar1, cmp)
isSorted(ar2, cmp)
isSorted(ar3, cmp)
isSorted(ar4, cmp)
isSorted(ar5, cmp)
