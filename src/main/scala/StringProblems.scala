object StringProblems {

    def LcSubStr(first: String, second: String): Option[Int] = {
        // Memoize the results to improve performance
        val memoizedResults = Array.ofDim[Int](first.length, second.length)

        def compare(pos1: Int, pos2: Int): Int = {
            var result = 0

            if (memoizedResults(pos1)(pos2) != null)
                result = memoizedResults(pos1)(pos2)

            if (pos1 == 0 || pos2 == 0)
                result = 0

            if (first.charAt(pos1 - 1).equals(second.charAt(pos2 - 1)))
                result = 1 + compare(pos1 - 1, pos2 - 1)

            if (!first.charAt(pos1 - 1).equals(second.charAt(pos2 - 1)))
                result = compare(pos1 - 1, pos2).max(compare(pos1, pos2 - 1))

            memoizedResults(pos1)(pos2) = result
            result
        }

        Option(compare(first.length, second.length))
    }

}
