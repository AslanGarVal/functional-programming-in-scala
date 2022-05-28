object Chapter3Exercises extends App {
  /*
  EXERCISE 3.1:
  What will be the result of the following expression
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  -> This returns 3 since the block mathces the third case
   */

  val x = List(1,2,3,4,5) match {
    case x :: 2 :: 4 :: _ => x
    case Nil => 42
    case x :: y :: 3 :: 4 :: _ =>x+y
    case h :: t => h + t.sum
    case _ => 101
  }
  println(x) // should equal 3

  /*
  EXERCISE 3.2:
  Implement the function tail for removing the first element of a List. Note that the
  function takes constant time. What are different choices you could make in your
  implementation if the List is Nil? We’ll return to this question in the next chapter.
   */
  def getTail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: t => t
  }

  /*
  EXERCISE 3.3:
  Using the same idea, implement the function setHead for replacing the first element
  of a List with a different value.
   */
  def setHead[A](l: List[A], value: A): List[A] = l match {
    case Nil => value :: Nil
    case _ :: t => value :: t
  }

  /*
  EXERCISE 3.4:
  Generalize tail to the function drop, which removes the first n elements from a list.
  Note that this function takes time proportional only to the number of elements being
  dropped—we don’t need to make a copy of the entire List.
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case _ :: next => drop(next, n - 1)
      case Nil => Nil
    }

  val myList = (1 to 10).toList
  println(drop(myList, 5))

  /*
  EXERCISE 3.5:
  Implement dropWhile, which removes elements from the List prefix as long as they
  match a predicate.
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: tail => if (f(h)) dropWhile(tail, f) else h :: tail
  }

  println(dropWhile(myList, (x: Int) => x > 10))
  println(dropWhile(myList, (x: Int) => x < 5))

  /*
  EXERCISE 3.6:
  Not everything works out so nicely. Implement a function, init, that returns a List
  consisting of all but the last element of a List. So, given List(1,2,3,4), init will
  return List(1,2,3). Why can’t this function be implemented in constant time like
  tail?

  -> This cannot be implemented in constant time since, unlike the case of head, the last element must
  be calculated by iterating over the whole length of the list
   */
  def init[A](l: List[A]): List[A] = l match {
    case x :: y :: Nil => x :: Nil
    case h :: tail => h :: init(tail)
  }

  println(init(myList))



  sealed trait Tree[+A] {
    /*
    EXERCISE 3.25:
    Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
    def size: Int = this match {
      case Leaf(1) => 1
      case Branch(left, right) => 1 + left.size + right.size
    }

    /*
    EXERCISE 3.27:
    Write a function depth that returns the maximum path length from the root of a tree
    to any leaf.
    */

    def length(): Int = this match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (left.length max right.length)
    }

    /*
    EXERCISE 3.28
    Write a function map, analogous to the method of the same name on List,
    that modifies each element in a tree with a given function.
     */

    def map[B](f: A => B): Tree[B] = this match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(left.map(f), right.map(f))
    }

    /*
    EXERCISE 3.29:
    Generalize size, maximum, depth, and map, writing a new function fold that abstracts
    over their similarities. Reimplement them in terms of this more general function. Can
    you draw an analogy between this fold function and the left and right folds for List?
     */

    //The key is to implement fold with two function arguments, one for each instance of a Tree:
    def fold[B](f: A => B, g: (B, B) => B): B = this match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))
    }

    def sizeWithFold: Int = fold(_ => 1, (l, r) => 1 + l + r)
    def lengthWithFold: Int = fold(_ => 0, (x: Int, y: Int) => 1 + (x max y))
    def mapWithFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))
  }
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  val aTree: Tree[Int] = Leaf(1)
  val anotherTree: Tree[Int] = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(1)))

  println(aTree.size)
  println(anotherTree.size)

  /*
  EXERCISE 3.26:
  Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  and y.)
   */
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  println(max(aTree))
  println(max(anotherTree))






}
