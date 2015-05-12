package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt
    (if(n < 0) -(n+1) else n, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n,newRng) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,rng2) = rng.nextInt
    val (d,rng3) = double(rng2)
    ( (i,d), rng3 )
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), newRng) = intDouble(rng)
    ((d,i),newRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(c: Int, rng: RNG, acc: List[Int]): (List[Int],RNG) =
      if(c > 0) {
        val (i,newRng) = rng.nextInt
        loop(c-1,newRng,i::acc)
      } else
        (acc,rng)

    loop(count,rng,Nil)
  }

  val doubleWithMap: Rand[Double] = {
    map(nonNegativeInt){
      n => n / (Int.MaxValue.toDouble + 1)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a,rngA) = ra(rng)
    val (b,rngB) = rb(rngA)
    (f(a,b),rngB)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())){
      case (i,acc) => map2(i,acc)((a,b) => a::b)
    }
  }

  def intsWithSequence(count: Int): Rand[List[Int]] = {
    val randInt: Rand[Int] = _.nextInt
    sequence(List.fill(count)(randInt))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a,rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    case i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) {
      case a => map(rb)(b => f(a, b))
    }

}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, newS) = run(s)
    f(a).run(newS)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S,List[A]](List())){
      case (i,acc) => i.map2(acc)((a,b) => a::b)
    }
  }

  def applyInput(input: Input, m: Machine): Machine = (m,input) match {
    case (Machine(_,candies,coins),Coin) if candies > 0 => Machine(false, candies,coins+1)
    case (Machine(false,candies,coins),Turn) => Machine(true,candies-1,coins)
    case (m@Machine(_,_,_),_) => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { m =>
    val finalM = inputs.foldLeft(m)((s, i) => applyInput(i,s))
    ((finalM.candies,finalM.coins),finalM)
  }
}
