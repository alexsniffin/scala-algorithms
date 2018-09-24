import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, GivenWhenThen}

class StringProblemsSpec extends FlatSpec with MockFactory with GivenWhenThen {
    info("Running GraphSpec")

    "LCS" should "return the longest substring" in {
        Given("two strings")
        val str1 = "AAB"
        val str2 = "BAA"

        When("comparing the two strings for common a substring")
        val result = StringProblems.LcSubStr(str1, str2)

        Then("the result should be 2")
        assert(result.get == 2)
    }
}
