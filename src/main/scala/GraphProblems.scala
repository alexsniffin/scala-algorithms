import scala.annotation.tailrec
import scala.collection.mutable

case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]])

object GraphProblems {

	/**
	  * breadth first search for tree
	  */
	def searchTree[T](tree: Node[T], value: T): Option[T] = {
		// side-effects contained only in this function
		val queue: mutable.Queue[Option[Node[T]]] = mutable.Queue[Option[Node[T]]]()

		/**
		  * tail recursion
		  */
		@tailrec
		def search(curNode: Option[Node[T]]): Option[Node[T]] = curNode match {
			case Some(node) => {
				if (node.value.equals(value)) return curNode

				queue.enqueue(node.left)
				queue.enqueue(node.right)

				search(queue.dequeue)
			}
			case None => if (queue.nonEmpty) search(queue.dequeue) else None
			case _ => None
		}

		Option(search(Option(tree)).get.value)
	}

}