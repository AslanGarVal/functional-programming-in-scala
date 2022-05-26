import scala.annotation.tailrec

object Chapter2Exercises extends App {
  /*
  EXERCISE 2.1:
  Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
  The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
  previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
  local tail-recursive function.
   */

  def fibonacci(n: Int): Int = {
    @tailrec
    def loop(n: Int, first: Int, second: Int): Int = {
      if (n == 1) first
      else loop(n - 1, second, first + second)
    }

    loop(n, 0, 1)
  }

  println((1 to 10).map(fibonacci(_)))

  /*
  EXERCISE 2.2:
  Implement isSorted, which checks whether an Array[A] is sorted according to a
  given comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n), as(n - 1))) false
      else loop(n + 1)
    }

    loop(1)
  }

  val sortedIntArray = Array(1, 2, 3, 4, 5)
  val unsortedIntArray = Array(1, 3, 2, 4, 4)
  println(isSorted(sortedIntArray, (x: Int, y: Int) => x > y))
  println(isSorted(unsortedIntArray, (x: Int, y: Int) => x > y))

  val sortedStringArray = Array("I", "am", "Paris")
  val unsortedStringArray = Array("Tokio", "is", "great")
  println(isSorted(sortedStringArray, (a: String, b: String) => a.length > b.length))
  println(isSorted(unsortedStringArray, (a: String, b: String) => a.length > b.length))

  /*
  EXERCISE 2.3:
  Let’s look at another example, currying, which converts a function f of two arguments
  into a function of one argument that partially applies f. Here again there’s only one
  implementation that compiles. Write this implementation.
   */

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => f(a: A, _: B)

  val f: (Int, Int) => Int = (x: Int, y: Int) => x + y
  println(curry(f)(1)(2))

  /*
  EXERCISE 2.4:
  Implement uncurry, which reverses the transformation of curry. Note that since =>
  associates to the right, A => (B => C) can be written as A => B => C.
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  val curriedF = (a: Int) => (b: Int) => a + b
  println(uncurry(curriedF)(1, 2))

  /*
  EXERCISE 2.5:
  Implement the higher-order function that composes two functions.
   */
  def compose[A, B, C](f: A => B, g: B => C): A => C = (x: A) => g(f(x))

  val f1: String => Int = (s: String) => s.length
  val g1: Int => String = (x: Int) => s"This number equals $x"
  println(compose(f1, g1)("hello, Scala"))
}
