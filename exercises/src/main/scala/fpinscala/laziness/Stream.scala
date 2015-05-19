package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,t) if n == 1 => cons(h(),empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case s => s
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((i,acc) => p(i) && acc)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((i,acc) => if(p(i)) cons(i,acc) else acc)

  def headOption: Option[A] =
    foldRight(None:Option[A])((i,acc) => Some(i))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((i,acc) => cons(f(i),acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((i,acc) => if(p(i)) cons(i,acc) else acc)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((i,acc) => cons(i,acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((i,acc) => f(i) append acc)

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Empty => None
      case Cons(h,t) => Some( (f(h()),t()) )
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this,n)){
      case (Cons(h,t),1) => Some( (h(),(empty,0)))
      case (Cons(h,t),x) if x > 1 => Some( (h(),(t(),x-1)) )
      case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h,t) if p(h()) => Some((h(),t()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWithWithUnfold(s2)((_,_))

  def zipWithWithUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this,s2)){
      case (Cons(h1,t1),Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this,s2)){
      case (Empty,Empty) => None
      case (Empty,Cons(h2,t2)) => Some((
        (None,Some(h2())),
        (Empty,t2())
        ))
      case (Cons(h1,t1),Empty) => Some((
        (Some(h1()),None),
        (t1(),Empty)
        ))
      case (Cons(h1,t1),Cons(h2,t2)) => Some((
        (Some(h1()),Some(h2())),
        (t1(),t2())
        ))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (a,b) => a == b
    }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s@Cons(_,t) => Some((s,t()))
    } append empty

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  val fibs: Stream[Int] = {
    def loop(prev: Int,cur: Int): Stream[Int] =
      cons(prev, loop(cur,cur+prev))

    loop(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a,s)) => cons(a,unfold(s)(f))
  }

  val fibsWithUnfold: Stream[Int] =
    unfold((0,1)){
      case (prev,cur) => Some((prev, (cur,prev+cur)))
    }

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i+1)))

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a,a)))

  val onesWithUnfold: Stream[Int] =
    unfold(1)(_ => Some((1,1)))
}