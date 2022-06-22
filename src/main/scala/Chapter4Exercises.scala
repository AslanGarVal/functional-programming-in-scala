object Chapter4Exercises extends App {

  /*
  EXERCISE 4.1:
  Implement map, flatMap, getOrElse, orElse and filter
   */
  /*
  sealed trait myOption[+A] {
    //extract value from option, then transform
    def map[B](f: A => B): myOption[B] = this match {
      case myNone => myNone
      case mySome(v) => mySome(f(v))
    }

    //extract value from option, transform, then repackage
    def flatMap[B](f: A => myOption[B]): myOption[B] = map(f).getOrElse(None)

    //if extraction fails, return a default
    def getOrElse[B >: A](default: => B): B = this match {
      case myNone => default
      case mySome(v) => v
    }

    def orElse[B >: A](ob: => myOption[B]): myOption[B] = map(mySome(_)).getOrElse(ob)

    def filter(f: A => Boolean): myOption[A] = flatMap(v => if (f(v)) mySome(v) else myNone)
  }

  case object myNone extends myOption[Nothing]
  case class mySome[A](v: A) extends myOption[A]

  /*
  EXERCISE 4.2:
  Implement the variance function in terms of flatMap. If the mean of a sequence is m,
  the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  See the definition of variance on Wikipedia (http://mng.bz/0Qsr)
   */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Option(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

   */
}