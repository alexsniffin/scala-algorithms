import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]])

object GraphProblems {

	/**
	  * breadth first search for tree
	  */
	def searchTree[T](tree: Node[T], value: T): Option[T] = {
		var queue: Queue[Node[T]] = Queue[Node[T]](tree)

		/**
		  * tail recursion
		  */
		@tailrec
		def search(curNode: Option[Node[T]]): Option[Node[T]] = curNode match {
			case Some(node) => {
				if (node.value.equals(value)) return curNode

				queue.enqueue(node.left)
				queue.enqueue(node.right)

				val next = queue.dequeue

				search(Option(next._1))
			}
			case None => None
			case _ => None
		}

		val dequeue = queue.dequeue
		queue = dequeue._2

		Option(search(Option(dequeue._1)).get.value)
	}

}