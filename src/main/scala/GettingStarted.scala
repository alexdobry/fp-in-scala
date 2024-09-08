import scala.annotation.tailrec

object GettingStarted {
  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n - 1)

  def factorial2(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n == 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // exercise
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, current: Int): Int = n match {
      case 0 => current
      case 1 => prev
      case n => go(n - 1, prev + current, prev)
    }

    go(n, 1, 0)
  }

  private def formatFib(n: Int) = {
    val msg = "The fib value of %d is %d."
    msg.format(n, fib(n))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }


  /*
    The two functions, formatAbs and formatFactorial, are almost identical.
    If we like, we can generalize these to a single function, formatResult,
    which accepts as an argument the function to apply to its argument:

    formatResult("fib value", 10, fib)
    formatResult("factorial", 7, factorial)
   */
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def go(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else go(n + 1)

    go(0)
  }

  /*
    What’s important is that the code for findFirst will look almost identical
    if we’re searching for a String in an Array[String], an Int in an Array[Int],
    or an A in an Array[A] for any given type A. We can write findFirst more
    generally for any type A by accepting a function to use for testing a particular
    A value.
   */
  def findFirst[A](xs: Array[A], p: (A) => Boolean): Int = {
    @tailrec
    def go(n: Int): Int =
      if (n >= xs.length) -1
      else if (p(xs(n))) n
      else go(n + 1)

    go(0)
  }

  // exercise
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int, s: Boolean): Boolean = {
      if (as.isEmpty) true
      else if (n >= as.length - 1) true
      else go(n + 1, ordered(as(n), as(n + 1)) && s)
    }

    go(0, s = true)
  }

  // exercise
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  // exercise
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // exercise
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    val f: (Int, Int) => Boolean = (a, b) => a < b
    assert(isSorted(Array(), f), true)
    assert(isSorted(Array(1), f), true)
    assert(isSorted(Array(1, 2), f), true)
    assert(isSorted(Array(2, 1), f), false)
  }
}
