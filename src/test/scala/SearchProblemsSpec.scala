import org.scalatest._
import org.scalamock.scalatest.MockFactory

class SearchProblemsSpec extends FlatSpec with MockFactory with GivenWhenThen {
    info("Running GraphSpec")

    "BFS for trees" should "find the given value" in {
        Given("a tree with nodes")
        val findValue = 1
        val treeFixture = new TreeNode[Int](0, right = Option(new TreeNode[Int](2, right = Option(new TreeNode[Int](1)))))

        When("searching the tree for a value")
        val result = SearchProblems.bfsTreeSearch[Int](treeFixture, findValue)

        Then("the result should be the value")
        assert(result.get.value.equals(findValue))
    }

    "BFS for trees" should "not find the given value" in {
        Given("a tree with nodes")
        val findValue = 1
        val treeFixture = new TreeNode[Int](0, right = Option(new TreeNode[Int](2, right = Option(new TreeNode[Int](3)))))

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
            Option(new GraphNode[Int](3)),
            Option(new GraphNode[Int](4)),
            Option(new GraphNode[Int](6, Option(Seq(
                Option(new GraphNode[Int](8)),
                Option(new GraphNode[Int](2, Option(Seq(
                    Option(new GraphNode[Int](1)),
                    Option(new GraphNode[Int](10))
                )))),
                Option(new GraphNode[Int](9))
            )))))
        ))

        When("searching the graph for a value")
        val result = SearchProblems.bfsGraphSearch[Int](graphFixture, findValue)

        Then("the result should be the value")
        assert(result.get.value.equals(findValue))
    }

    "BFS for graphs" should "not find the given value" in {
        Given("a graph with nodes")
        val findValue = 1
        val graphFixture = new GraphNode[Int](0, Option(Seq(
            Option(new GraphNode[Int](3)),
            Option(new GraphNode[Int](4)),
            Option(new GraphNode[Int](6, Option(Seq(
                Option(new GraphNode[Int](8)),
                Option(new GraphNode[Int](2, Option(Seq(
                    Option(new GraphNode[Int](11)),
                    Option(new GraphNode[Int](10))
                )))),
                Option(new GraphNode[Int](9))
            )))))
        ))

        When("searching the graph for a value")
        val result = SearchProblems.bfsGraphSearch[Int](graphFixture, findValue)

        Then("the result should be none")
        assert(result.isEmpty)
    }

    "DFS for trees" should "find the given value" in {
        Given("a tree with nodes")
        val findValue = 1
        val treeFixture = new TreeNode[Int](0, right = Option(new TreeNode[Int](2, right = Option(new TreeNode[Int](1)))))

        When("searching the tree for a value")
        val result = SearchProblems.dfsTreeSearch[Int](treeFixture, findValue)

        Then("the result should be the value")
        assert(result.get.value.equals(findValue))
    }

    "DFS for trees" should "not find the given value" in {
        Given("a tree with nodes")
        val findValue = 1
        val treeFixture = new TreeNode[Int](0, right = Option(new TreeNode[Int](2, right = Option(new TreeNode[Int](3)))))

        When("searching the tree for a value")
        val result = SearchProblems.dfsTreeSearch[Int](treeFixture, findValue)

        Then("the result should be none")
        assert(result.isEmpty)
    }

    "DFS for graphs" should "find the given value" in {
        //work around for mock
        //https://github.com/paulbutcher/ScalaMock/issues/221


        Given("a graph with nodes")
        val findValue = 1
        val graphFixture = new GraphNode[Int](0, Option(Seq(
            Option(new GraphNode[Int](3)),
            Option(new GraphNode[Int](4)),
            Option(new GraphNode[Int](6, Option(Seq(
                Option(new GraphNode[Int](8)),
                Option(new GraphNode[Int](2, Option(Seq(
                    Option(new GraphNode[Int](1)),
                    Option(new GraphNode[Int](10))
                )))),
                Option(new GraphNode[Int](9))
            )))))
        ))

        When("searching the graph for a value")
        val result = SearchProblems.dfsGraphSearch[Int](graphFixture, findValue)

        Then("the result should be the value")
        assert(result.get.value.equals(findValue))
    }

    "DFS for graphs" should "not find the given value" in {
        Given("a graph with nodes")
        val findValue = 1
        val graphFixture = new GraphNode[Int](0, Option(Seq(
            Option(new GraphNode[Int](3)),
            Option(new GraphNode[Int](4)),
            Option(new GraphNode[Int](6, Option(Seq(
                Option(new GraphNode[Int](8)),
                Option(new GraphNode[Int](2, Option(Seq(
                    Option(new GraphNode[Int](11)),
                    Option(new GraphNode[Int](10))
                )))),
                Option(new GraphNode[Int](9))
            )))))
        ))

        When("searching the graph for a value")
        val result = SearchProblems.dfsGraphSearch[Int](graphFixture, findValue)

        Then("the result should be none")
        assert(result.isEmpty)
    }
}
