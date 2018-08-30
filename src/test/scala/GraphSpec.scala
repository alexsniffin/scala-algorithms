import org.scalatest._

class GraphSpec extends FlatSpec with GivenWhenThen {

	info("Running GraphSpec")

	"BFS for trees" should "find the given value" in {
		Given("a tree with nodes")
		val findValue = 1
		val treeFixture = new Node[Int](0, left = None, right = Option(new Node[Int](2, left = None, right = Option(new Node[Int](1, left = None, right = None)))))

		When("searching the tree for a value")
		val result = GraphProblems.searchTree(treeFixture, findValue)

		Then("the result should be the value")
		assert(result.get.equals(findValue))
	}

}
