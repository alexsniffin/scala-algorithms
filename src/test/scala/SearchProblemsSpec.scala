import org.scalatest._
import org.scalamock.scalatest.MockFactory

class SearchProblemsSpec extends FlatSpec  with MockFactory with GivenWhenThen {
    info("Running GraphSpec")

    "BFS for trees" should "find the given value" in {
        Given("a tree with nodes")
        val findValue = 1
        val treeFixture = new TreeNode[Int](0, left = None, right = Option(new TreeNode[Int](2, left = None, right = Option(new TreeNode[Int](1, left = None, right = None)))))

        When("searching the tree for a value")
        val result = SearchProblems.bfsTreeSearch[Int](treeFixture, findValue)

        Then("the result should be the value")
        assert(result.get.equals(findValue))
    }

    "BFS for trees" should "not find the given value" in {
        Given("a tree with nodes")
        val findValue = 1
        val treeFixture = new TreeNode[Int](0, left = None, right = Option(new TreeNode[Int](2, left = None, right = Option(new TreeNode[Int](3, left = None, right = None)))))

        When("searching the tree for a value")
        val result = SearchProblems.bfsTreeSearch[Int](treeFixture, findValue)

        Then("the result should be none")
        assert(result.isEmpty)
    }

    "BFS for graphs" should "find the given value" in {
        //work around for mock
        //https://github.com/paulbutcher/ScalaMock/issues/221


        Given("a graph with nodes")
        val findValue = 1
        val graphFixture = new GraphNode[Int](0, Option(Seq(
            Option(new GraphNode[Int](3, None)),
            Option(new GraphNode[Int](4, None)),
            Option(new GraphNode[Int](6, Option(Seq(
                Option(new GraphNode[Int](8, None)),
                Option(new GraphNode[Int](2, Option(Seq(
                    Option(new GraphNode[Int](1, None)),
                    Option(new GraphNode[Int](10, None))
                )))),
                Option(new GraphNode[Int](9, None))
            )))))
        ))

        When("searching the graph for a value")
        val result = SearchProblems.bfsGraphSearch[Int](graphFixture, findValue)

        Then("the result should be the value")
        assert(result.get.equals(findValue))
    }

    "BFS for graphs" should "not find the given value" in {
        Given("a graph with nodes")
        val findValue = 1
        val graphFixture = new GraphNode[Int](0, Option(Seq(
            Option(new GraphNode[Int](3, None)),
            Option(new GraphNode[Int](4, None)),
            Option(new GraphNode[Int](6, Option(Seq(
                Option(new GraphNode[Int](8, None)),
                Option(new GraphNode[Int](2, Option(Seq(
                    Option(new GraphNode[Int](11, None)),
                    Option(new GraphNode[Int](10, None))
                )))),
                Option(new GraphNode[Int](9, None))
            )))))
        ))

        When("searching the graph for a value")
        val result = SearchProblems.bfsGraphSearch[Int](graphFixture, findValue)

        Then("the result should be none")
        assert(result.isEmpty)
    }
}
