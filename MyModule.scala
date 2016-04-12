object MyModule {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  /** Excercise 2.1 */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n:Int, a: Int, b: Int): Int =
      if (n <= 1) b
      else go(n-1, b, a+b)

    if (n <= 1) n
    else go(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  /** Excercise 2.2 */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  /**
   * Take a value and a function of two arguments, and return a functsin of one arguments.
   * We are applying the function to soeme but not all of the arguments.
   */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  /** 
   * Exercise 2.3 
   * Currying takes a function `f` for two arguments into a function of one argument
   * that partially applies `f`.
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {

  }

}
