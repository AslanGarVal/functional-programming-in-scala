import scala.annotation.tailrec

object Chapter6Exercises extends App {
  trait RNG {
    def nextInt: (Int, RNG) //return result AND current state
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL //create new seed for new state
      val nextRNG = SimpleRNG(newSeed) //instantiate new state
      val n = (newSeed >>> 16).toInt //generate the actual new random int

      (n, nextRNG)
    }
  }

  //Exercise 5.1: Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.MaxValue
  def nonNegativeValueInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  //Exercise 5.2: Write a function to generate a Double between 0 and 1, not including 1.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeValueInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  //Exercise 5.3: Write functions to generate an (Int, Double) pair, and (Int, Double) pair and a
  // (Double, Double, Double) triple.

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)

    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  //Exercise 5.4: Write a function to generate a List of random Ints
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def traverse(acc: List[Int], n: Int)(rng: RNG): (List[Int], RNG) = {
      if (n == 1) {
        (acc, rng)
      } else {
        val (i, r) = rng.nextInt
        traverse(i :: acc, n - 1)(r)
      }
    }

    traverse(Nil, count)(rng)
  }

  //alias for state transitions
  type Rand[+A] = RNG => (A, RNG)

  //passes the current state along without using it
  def unit[A](a : A): Rand[A] = rng => (a, rng)

  //modifies the output without modifying the state itself
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r2) = s(rng)
    (f(a), r2)
  }

  //example: nonNegativeEven
  def nonNegativeEven: Rand[Int] = map(nonNegativeValueInt)(i => i - i % 2)

  //Exercise 6.5: Use map to implement double in a more elegant way
  def doubleWithMap: Rand[Int] = map(nonNegativeValueInt)(i => i / (Int.MaxValue + 1))

  //Exercise 6.6: Write the implementation of map2 based on the signature.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)

    (f(a,b), r2)
  }

  //we can express the combination of two values succinctly
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
  //for example: intDouble and doubleInt
  def int: Rand[Int] = _.nextInt
  def intDoubleWithBoth: Rand[(Int, Double)] = both(int, double)
  def doubleInt: Rand[(Double, Int)] = both(double, int)

  //Exercise 6.7: If you combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a list of transitions into a single transition. Use it to reimplement the ints
  // function you wrote before
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  //Exercise 6.8: Implement flatMap, and then use it to implement nonNegativeLessThan
  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = s(rng)
    f(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeValueInt) {i =>
    val mod = i % n
    if (i + (n + 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }
  val r: RNG = SimpleRNG(42)

  //Exercise 6.8: Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we're referring
  // to when we say that flatMap is more powerful than map or mpa2.
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a =>
    mapWithFlatMap(rb)(b => f(a, b)))
  println(ints(10)(r)._1)

  println(sequence(List(unit(1), unit(2), unit(3)))(r)._1)
  println(_ints(10)(r)._1)


  //We can generalise past the simple RNG example, and notice that we can abstract to state transitions in general

  /*
   S: the state that is used to generate the value of type A
   run: the state transition (the way we generate a value from current state and how does that modify state)
   */
  case class State[S, +A](run: S => (A, S)) {
    //exercise 6.9: Generalise the functions unit, map, map2, flatMap and sequence. Add them as methods on the State
    // case class where possible. Otherwise put them in a companion object.
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, sNext) = run(s)
      f(a).run(sNext)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => s.map(b => f(a, b)))


  }

  object State {
    def unit[S, A](a: A): State[S, A] = State((a, _))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(State.unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }


  //Exercise 6.10: To gain experience with the use of State, implement a finite state automaton
  //that models a simple candy dispenser. The machine has two types of input: you can
  //insert a coin, or you can turn the knob to dispense candy. It can be in one of two
  //states: locked or unlocked. It also tracks how many candies are left and how many
  //coins it contains.

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(isLocked: Boolean, candies: Int, coins: Int) {
    def operate(input: Input):  ((Int, Int), Machine) = if (candies > 0) input match {
      case Coin if isLocked => ((candies, coins + 1), Machine(false, candies, coins + 1))
      case Turn if !isLocked => ((candies - 1, coins), Machine(!isLocked, candies - 1, coins))
      case _ => ((candies, coins), this)
    } else ((candies, coins), this)
  }

  def run(input: Input): Machine => ((Int, Int), Machine) = _.operate(input)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(inputs.map(i => State(run(i)))).map(_.last)

  val testInput: List[Input] = List(Coin, Turn, Turn, Turn, Coin, Turn, Coin, Coin)
  val testMachine: Machine = Machine(isLocked = true, 10, 0)
  val state: State[Machine, (Int, Int)] = simulateMachine(testInput)

  println(state.run(testMachine)._1)


}
