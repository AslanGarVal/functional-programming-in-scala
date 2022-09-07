object Chapter7Exercises extends App {
  //Goal: Create a purely functional parallel and asynchronic library

  /*
    We usually start with a general idea of what we want to do.
    We must refine that concept and find a data type that enables
    the desired functionality.

    e.g.: We want to create parallel computations. What is that?
    We can start with summing a list of integers in parallel.
   */

  // naive way of summing:
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)(_ + _)

  // we can use a "divide and conquer" algorithm instead. This can be parallelised
  def sum(ints: IndexedSeq[Int]): Int = {
    // IndexedSeq is a superclass of sequences that allows efficient seq splitting
    if (ints.size <= 1) // base case
      ints.headOption getOrElse 0
    else {
      // split in half and recursively apply to each half
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }

  /*
    We start with an ideal overview of how the API must be designed.
    Implementation will come later.

    From our example, we notice that any used data type must be able
    to contain results. The result will have a type (e.g. Int) and it
    must be able to be extracted.

    We can just invent a container type for our result:
   */

  sealed trait Par[A] {

    // extracts the resulting value from a parallel computation
    def get[A](a: Par[A]): A

    // takes an unevaluated A and returns a computation that might evaluate it later
    def unit[A](a: => A): Par[A]

    /*
      EXERCISE 7.1: Par.map2 is a new higher-order function for combining
      the result of two parallel computations. What is its signature?
     */

    def map2[A, B, C](par1: Par[A], par2: Par[B])(f: (A, B) => C): Par[C]
  }




  // now our sum example looks like
  /*
   def parSum1(ints: IndexedSeq[Int]): Int =
    if (ints.size >= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l)) // compute left in parallel
      val sumR: Par[Int] = Par.unit(sum(r))

      Par.get(sumL) + Par.get(sumR)
    }
   */

  /*
    Notice however that we have two options in regard of the evaluation
    when unit and get are invoked:

    1. Computation starts concurrently as soon as unit is called
    2. Computation is delayed until get is called

    2 breaks parallelism since we would make the sum of the two gets
    sequential if we wait for the first one before the second one is complete.

    1 breaks referential transparency since calling
    Par.get(Par.unit) will simply wait until the computation is done in unit
    before calling get;  this breaks parallelism again.

    Whenever we call unit, we get a Par. However, as soon as we call get
    we wait for the "side effect" of the computation to take place.
    As such we need to be able to delay side effects until the very end.
    This makes the combination of parallel computations a requirement.
   */

  // an example of summation with combination of parallel computations looks like:
  /*
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size >= 1)
      Par.unit(ints.headOption getOrElse 0)
     else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
     }
   */

  //
}
