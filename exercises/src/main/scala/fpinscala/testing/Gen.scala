package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {

  def check: Either[(FailedCase,SuccessCount), SuccessCount]

//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = this.check && p.check
//  }

}

object Prop {
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  val boolGen: Gen[Boolean] = Gen(
    State(_.nextInt match {
      case (n,rng) => (n%2 == 0,rng)
    })
  )

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start))
  )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolGen.flatMap(b => if(b) g1 else g2)

}

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_,this))

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

