import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, GivenWhenThen}

class StringProblemsSpec extends FlatSpec with MockFactory with GivenWhenThen {
    info("Running StringProblemsSpec")

    "LCS" should "return the longest substring" in {
        Given("two strings")
        val str1 = "AAB"
        val str2 = "BAA"

        When("comparing the two strings for a common substring")
        val result = StringProblems.LcSubStr(str1, str2)

        Then("the result should be 2")
        assert(result.get == 2)
    }

	"LCS" should "return the longest substring with multiple common sub strings" in {
		Given("two strings with multiple common sub strings")
		val str1 = "AABAAA"
		val str2 = "BAABAA"

		When("comparing the two strings for the longest common substring")
		val result = StringProblems.LcSubStr(str1, str2)

		Then("the result should be 3")
		assert(result.get == 5)
	}
}
