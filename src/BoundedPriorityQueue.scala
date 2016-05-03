
import scala.reflect.ClassTag
import java.util.PriorityQueue

class BoundedPriorityQueue[T](maxSize: Int)(implicit order: Ordering[T]) 
    extends java.io.Serializable {
    
    protected var theQueue = new PriorityQueue[T](order)
    @transient protected lazy val downMethod = {
        val method = classOf[PriorityQueue[T]].getDeclaredMethod("siftDown", classOf[Int], classOf[Object])
        method.setAccessible(true)
        method
    }

    protected def elements() = {
        val field = classOf[PriorityQueue[T]].getDeclaredField("queue")
        field.setAccessible(true)
        field.get(theQueue).asInstanceOf[Array[Object]]
    }

    protected def downTop(elem: T) =
        downMethod.invoke(theQueue, 0.asInstanceOf[Object], elem.asInstanceOf[Object]).asInstanceOf[Boolean]

    def add(elem: T) = {
        if (theQueue.size == maxSize) {
            if (order.compare(theQueue.peek, elem) < 0)
                downTop(elem)
            else
                false
        } else
            theQueue.add(elem)
    }

    def +=(elem: T) = add(elem)

    def length() = theQueue.size

    def size() = theQueue.size

    def toArray()(implicit tag: ClassTag[T]) = theQueue.toArray.map(_.asInstanceOf[T]).toArray

    /**
     * move elements in another bounded priority queue to this bounded priority queue
     * note: another priority queue will be empty
     */
    def moveIn(b: BoundedPriorityQueue[T]): BoundedPriorityQueue[T] = {
        if (size < b.size) {
            val tmp = theQueue
            theQueue = b.theQueue
            b.theQueue = tmp
        }
        for (i <- b.elements)
            add(i.asInstanceOf[T])
        b.theQueue.clear()
        this
    }

    def <=(b: BoundedPriorityQueue[T]) = moveIn(b)
}

object BoundedPriorityQueue {
    def empty[T](maxSize: Int)(implicit order: Ordering[T]): BoundedPriorityQueue[T] = new BoundedPriorityQueue[T](maxSize)(order)
}