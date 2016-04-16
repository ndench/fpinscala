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
    def go(n:Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else go(n-1, cur, prev + cur)

    go(n, 0, 1)
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
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!gt(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  /**
   * Take a value and a function of two arguments, and return a functsin of 
   * one arguments. We are applying the function to soeme but not all of the 
   * arguments.
   */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  /** 
   * Exercise 2.3 
   * Currying converts a function `f` for two arguments into a function of one
   * argument that partially applies `f`.
   *
   * We don't know or care where 'b' comes from, because that's a param 
   * required by the function returned, it will be provided when the returned 
   * function is called.
   * We know that f(a,b) will return type C.
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a,b))
  }

  /**
   * Exercise 2.4
   * Uncurry reverses curry. Fat arrow associates to the right so in 
   * A => (B => C) 
   * we don't need the brackets.
   *
   * We konw that f(a) will return a function of type B => C, 
   * so if we pass 'b' to to that function, will get a C.
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Exercise 2.5
   * Compose two functions together.
   *
   * We know that if we pass 'a' to 'g', we get a 'b'.
   * Pass that to 'f' and we get 'c'
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
