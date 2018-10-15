object MiscProblems {

    /**
      * Fibonacci
      *
      * @param n    value
      * @return     result
      */
    @throws(classOf[StackOverflowError])
    def fib(n: Int) : Option[Int] = n match {
        case 0 => Some(0)
        case 1 => Some(1)
        case cur: Int if cur > 1 => Some(fib(cur - 2).get + fib(cur - 1).get) // why am i so dumb
        case _ => None
    }

}
