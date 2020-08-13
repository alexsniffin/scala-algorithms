
import org.scalatest.{FlatSpec, GivenWhenThen}

class MiscProblemsSpec extends FlatSpec with GivenWhenThen {
  info("Running MiscProblemsSpec")

  "MiscProblems fib" should "give a valid fibonacci result" in {
    Given("a value for n")
    val n: Int = 10

    When("running fib")
    val result = MiscProblems.fib(n)

    Then("the return should be 55")
    assert(result.get == 55)
  }

  "MiscProblems fib" should "give a valid fibonacci result for base case 1" in {
    Given("a value for n")
    val n: Int = 1

    When("running fib")
    val result = MiscProblems.fib(n)

    Then("the return should be 1")
    assert(result.get == 1)
  }

  "MiscProblems fib" should "give a valid fibonacci result for base case 0" in {
    Given("a value for n")
    val n: Int = 0

    When("running fib")
    val result = MiscProblems.fib(n)

    Then("the return should be 0")
    assert(result.get == 0)
  }

  "MiscProblems fib" should "give a valid fibonacci result for negative" in {
    Given("a value for n")
    val n: Int = -1

    When("running fib")
    val result = MiscProblems.fib(n)

    Then("the return should be empty")
    assert(result.isEmpty)
  }

  "MiscProblems fib" should "give a valid fibonacci result for big int" in {
    Given("a value for n")
    val n: Int = Math.pow(2, 33).toInt

    When("running fib")
    Then("the return should throw an exception")
    assertThrows[StackOverflowError](MiscProblems.fib(n))
  }
}
