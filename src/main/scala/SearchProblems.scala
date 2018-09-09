import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.{HashSet, List}
import scala.language.postfixOps

case class TreeNode[T](value: T, left: Option[TreeNode[T]] = None, right: Option[TreeNode[T]] = None)
case class GraphNode[T](value: T, edges: Option[Seq[Option[GraphNode[T]]]] = None)

object SearchProblems {

    /***
      * Tail-recursive breadth first search for a binary tree
      * @param tree Node at the lowest level
      * @param value Value to find
      * @tparam T Type of the `value`
      * @return Option with the node
      */
	def bfsTreeSearch[T](tree: TreeNode[T], value: T): Option[TreeNode[T]] = {
		// Side-effects contained only in this function scope
		val queue: mutable.Queue[Option[TreeNode[T]]] = mutable.Queue[Option[TreeNode[T]]]()

		/**
		  * Tail recursion search
		  */
		@tailrec
		def search(curNode: Option[TreeNode[T]]): Option[TreeNode[T]] = curNode match {
			case Some(node) =>
				if (node.value.equals(value)) return Option(node)

				queue += node.left
				queue += node.right

				search(queue.dequeue)
			case None => if (queue.nonEmpty) search(queue.dequeue) else None
			case _ => None
		}

		Option(search(Option(tree))).flatten
	}

    /***
      * Tail-recursive breadth first search for a graph
      * @param graph Entry node
      * @param value Value to find
      * @tparam T Type of the `value`
      * @return Option with the node
      */
    def bfsGraphSearch[T](graph: GraphNode[T], value: T): Option[GraphNode[T]] = {
        // Side-effects contained only in this function scope
        val queue: mutable.Queue[Option[GraphNode[T]]] = mutable.Queue[Option[GraphNode[T]]]()
        val visited: mutable.HashSet[T] = mutable.HashSet[T]()

        /**
          * Tail recursion search
          */
        @tailrec
        def search(curNode: Option[GraphNode[T]]): Option[GraphNode[T]] = curNode match {
            case Some(node) =>
                if (node.value.equals(value)) return Option(node)

                if (!visited.contains(node.value) && node.edges.isDefined)
                    node.edges.get.foreach(edge => queue += edge)

                if (queue.isEmpty) None else {
                    visited += node.value
                    search(queue.dequeue)
                }
            case None => if (queue.nonEmpty) search(queue.dequeue) else None
            case _ => None
        }

        Option(search(Option(graph))).flatten
    }

    /***
      * Tail-recursive depth first search for a binary tree
      * @param tree Node at the lowest level
      * @param value Value to find
      * @tparam T Type of the `value`
      * @return Option with the node
      */
    def dfsTreeSearch[T](tree: TreeNode[T], value: T): Option[TreeNode[T]] = {
        var stack: List[Option[TreeNode[T]]] = List[Option[TreeNode[T]]]()

        /**
          * Tail recursion search
          */
        @tailrec
        def search(curNode: Option[TreeNode[T]]): Option[TreeNode[T]] = curNode match {
            case Some(node) =>
                if (node.value.equals(value)) return Option(node)

                // Append child nodes
                stack = node.left :: node.right :: stack

                val result = stack.head
                stack = stack.tail

                search(result)
            case None => if (stack.nonEmpty) {
                val result = stack.head
                stack = stack.tail

                search(result)
            } else None
            case _ => None
        }

        Option(search(Option(tree))).flatten
    }

    /***
      * Tail-recursive depth first search for a graph
      * @param graph Entry node
      * @param value Value to find
      * @tparam T Type of the `value`
      * @return Option with the node
      */
    def dfsGraphSearch[T](graph: GraphNode[T], value: T): Option[GraphNode[T]] = {
        // Side-effects contained only in this function scope
        var stack: List[Option[GraphNode[T]]] = List[Option[GraphNode[T]]]()
        var visited: HashSet[T] = HashSet[T]()

        /**
          * Tail recursion search
          */
        @tailrec
        def search(curNode: Option[GraphNode[T]]): Option[GraphNode[T]] = curNode match {
            case Some(node) =>
                if (node.value.equals(value)) return Option(node)

                if (!visited.contains(node.value) && node.edges.isDefined)
                    stack = node.edges.get.toList ::: stack

                if (stack.isEmpty) None else {
                    visited += node.value

                    val result = stack.head
                    stack = stack.tail

                    search(result)
                }
            case None => if (stack.nonEmpty) {
                val result = stack.head
                stack = stack.tail

                search(result)
            } else None
            case _ => None
        }

        Option(search(Option(graph))).flatten
    }
}