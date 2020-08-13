object StringProblems {

  /**
    * LCS with memoization for optimizing performance
    *
    * @param first  First input str
    * @param second Second input str
    * @return Option of Type Int that has the longest common substr length
    */
  def LcSubStr(first: String, second: String): Option[Int] = {
    // Memoize the results to improve performance
    val memoizedResults = Array.ofDim[Option[Int]](first.length + 1, second.length + 1)

    def compare(pos1: Int, pos2: Int): Int = {
      val result: Int = if (memoizedResults.length > pos1 &&
          memoizedResults(pos1).length > pos2 &&
          memoizedResults(pos1)(pos2) != null)
        memoizedResults(pos1)(pos2).get
      else if (pos1 == 0 || pos2 == 0)
        0
      else if (first.charAt(pos1 - 1).equals(second.charAt(pos2 - 1)))
        1 + compare(pos1 - 1, pos2 - 1)
      else if (!first.charAt(pos1 - 1).equals(second.charAt(pos2 - 1)))
        compare(pos1 - 1, pos2).max(compare(pos1, pos2 - 1))
      else 0

      memoizedResults(pos1)(pos2) = Option(result)
      result
    }

    Option(compare(first.length, second.length))
  }

}
