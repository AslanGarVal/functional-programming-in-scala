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
      case Cons(h, _) if n > 1 => cons(h(), take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t() drop (n - 1)
      case _ => this
    }

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

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


  val myStream = Stream(1, 2, 3, 4)
  println(myStream.toList)
}

