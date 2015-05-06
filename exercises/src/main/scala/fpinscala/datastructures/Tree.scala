package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(z: A => B)(f: (B,B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l,r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l,r) => 1 + l max r)

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)):Tree[B])(Branch(_,_))
}