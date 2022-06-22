import Chapter5Exercises.Stream.{cons, empty}

object Chapter5Exercises extends App {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h()) //explicit forcing to evaluate h
    }

    //exercise 5.1: Write a function to convert a Stream to a List
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    //exercise 5.2: Write the function take(n) for returning the first n elements of a stream and drop(n)
    // for skipping the first n elements of a stream

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t() drop (n - 1)
      case _ => this
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    //Exercise 5.3: Implement the function takeWhile for returning all starting elements of a Stream that match
    // the given predicate
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }

    def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    //Exercise 5.4: Implement forAll, which checks that all elements in the Stream match a given predicate.
    // Your implementation should terminate the traversal as soon as it encounters a non-matching value
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    //Exercise 5.5: Use foldRight to implement takeWhile
    def takeWhileWithFold(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)


    //Exercise 5.6: Implement headOption with foldRight
    def headOptionWithTakeWhile: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

    //Exercise 5.7: Implement map, filter, append and flatMap using foldRight. The append method should be non strict
    // in its argument
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
    def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)
    def append[B >: A](stream: => Stream[B]): Stream[B] = foldRight(stream)((h, t) => cons(h, t))
    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

    //Exercise 5.8: Generalise ones to slightly to the function constant, which returns an infinite Stream of constant
    // values
    def constant[A](v: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => v, () => tail)
      tail
    }

    //Exercise 5.10: Write a function fibs that generates the infinite stream of Fibonacci numbers
    def fibs: Stream[Int] = {
      def loop(n0: Int, n1: Int): Stream[Int] = cons(n0, loop(n1, n1 + n0))

      loop(0, 1)
    }

    //Exercise 5.11: Write a more general stream-building function called unfold. It takes an initial state
    // and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z : S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

    //Exercise 5.12: Write fibs, from, constant and ones in terms of unfold
    def fibsFromUnfold: Stream[Int] = unfold((0, 1)) {case (n0, n1) => Some(n0, (n1, n0 + n1))}
    def fromFromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))
    def constantFromUnfold[A](v: A): Stream[A] = unfold(v)(v => Some(v, v))
    def onesFromUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

    //Exercise 5.13: Use unfold to implement map, take, takeWhile, zipWith and zipAll
    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    def zipWith[B, C](stream2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, stream2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

    def zipAll[B](stream2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, stream2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Empty, Empty) => None
    }

    //Exercise 5.14: Implement startsWith using functions you've written. It should check if one Stream is
    // a prefix of another.

    def startsWith[A](stream2: Stream[A]): Boolean = zipWith(stream2)((a, b) => a == b).foldRight(true)(_ && _)

    //Exercise 5.15: Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the
    // input sequence, starting with the original Stream.

    def tails: Stream[Stream[A]] = cons(this, unfold(this) {
      case Empty => None
      case Cons(_, t) => Some(t(), t())
    })

    //Exercise 5.16: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of
    // the immediate results
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
  }



  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  //smart constructor for creating nonempty streams
  object Stream{
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl

      Cons(() => head, () => tail)
    }

    //Exercise 5.9: write a function that generates an infinite stream of integers, starting from n, then n + 1,
    // and so on.
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


  val myStream = Stream(1, 2, 3, 4)
  val myStream2 = Stream(1, 2)
  val myStream3 = Stream(1, 1)


  val x = Stream.from(1)
  val y = empty.fibs

  println(myStream.map(_ * 10).toList)
  println(myStream startsWith myStream2)
  println(myStream startsWith myStream3)

  println((myStream zipAll myStream2).toList)
  println(x.take(25).toList)
  println(x.zipWith(myStream3)(_ == _).toList)
  println(x startsWith myStream2)

  println(y startsWith myStream)
  println(y startsWith Stream(0, 1))

  println(myStream.tails.map(_.toList).toList)
  println(myStream.scanRight(0)(_+_).toList)
}

