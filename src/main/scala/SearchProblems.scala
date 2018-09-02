import scala.annotation.tailrec
import scala.collection.mutable

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
}