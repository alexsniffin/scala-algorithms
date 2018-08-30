import scala.annotation.tailrec
import scala.collection.mutable

case class TreeNode[T](value: T, left: Option[TreeNode[T]], right: Option[TreeNode[T]])
case class GraphNode[T](value: T, edges: Option[Seq[Option[GraphNode[T]]]])

object SearchProblems {

	/**
	  * Tail-recursive breadth first search for a binary tree
	  */
	def bfsTreeSearch[T](tree: TreeNode[T], value: T): Option[T] = {
		// Side-effects contained only in this function
		val queue: mutable.Queue[Option[TreeNode[T]]] = mutable.Queue[Option[TreeNode[T]]]()

		/**
		  * Tail recursion search
		  */
		@tailrec
		def search(curNode: Option[TreeNode[T]]): Option[T] = curNode match {
			case Some(node) =>
				if (node.value.equals(value)) return Option(node.value)

				queue.enqueue(node.left)
				queue.enqueue(node.right)

				search(queue.dequeue)
			case None => if (queue.nonEmpty) search(queue.dequeue) else None
			case _ => None
		}

		Option(search(Option(tree))).flatten
	}

    /**
      * Tail-recursive breadth first search for a graph
      */
    def bfsGraphSearch[T](graph: GraphNode[T], value: T): Option[T] = {
        // Side-effects contained only in this function
        val queue: mutable.Queue[Option[GraphNode[T]]] = mutable.Queue[Option[GraphNode[T]]]()

        /**
          * Tail recursion search
          */
        @tailrec
        def search(curNode: Option[GraphNode[T]]): Option[T] = curNode match {
            case Some(node) =>
                if (node.value.equals(value)) return Option(node.value)

                if (node.edges.isDefined) node.edges.get.foreach(edge => queue += edge)

                if (queue.isEmpty) None else search(queue.dequeue)
            case None => if (queue.nonEmpty) search(queue.dequeue) else None
            case _ => None
        }

        Option(search(Option(graph))).flatten
    }
}