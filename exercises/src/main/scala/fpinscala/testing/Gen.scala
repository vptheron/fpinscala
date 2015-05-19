package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop { (maxS, tc, rng) =>
    this.run(maxS, tc, rng) match {
      case f: Falsified => f
      case _ => p.run(maxS, tc, rng)
    }
  }

  def ||(p: Prop): Prop = Prop { (maxS, tc, rng) =>
    this.run(maxS, tc, rng) match {
      case f: Falsified => p.run(maxS, tc, rng)
      case p => p
    }
  }

}

object Prop {

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Proved extends Result {
    def isFalsified: Boolean = false
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  //  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
  //    forAll(g.forSize)(f)
  //
  //  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
  //    (max, n, rng) =>
  //      val casesPerSize = (n + (max - 1)) / max
  //      val props: Stream[Prop] =
  //        Stream.from(0).take((n min max) + 1)
  //          .map(i => forAll(g(i))(f))
  //      val prop: Prop =
  //        props.map(p => Prop { (max, _, rng) =>
  //          p.run(max, casesPerSize, rng)
  //        }).toList.reduce(_ && _)
  //      prop.run(max, n, rng)
  //  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  val boolGen: Gen[Boolean] = Gen(
    State(_.nextInt match {
      case (n, rng) => (n % 2 == 0, rng)
    })
  )

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
  )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf1[A](g: Gen[A]): Gen[List[A]] =
    listOfN(1,g)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolGen.flatMap(b => if (b) g1 else g2)

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => listOfN(size, g))

}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)

}

case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(
    forSize andThen (_ flatMap f)
  )

  def map[B](f: A => B): SGen[B] = SGen(
    forSize.andThen(genA => genA.map(f))
  )

}

