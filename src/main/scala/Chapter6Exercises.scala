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
  val r: RNG = SimpleRNG(42)
  println(ints(10)(r)._1)

  println(sequence(List(unit(1), unit(2), unit(3)))(r)._1)
  println(_ints(10)(r)._1)

}
