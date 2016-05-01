
import scala.reflect.ClassTag
import java.util.PriorityQueue

class BoundedPriorityQueue[T: ClassTag](maxSize: Int){
    protected var theQueue = new PriorityQueue[T]
    protected lazy val downMethod = {
        try {
            val method = classOf[PriorityQueue[T]].getDeclaredMethod("siftDown", classOf[Int], classOf[Object])
            method.setAccessible(true)
            method
        } catch {
            case e: NoSuchMethodException => throw e
        }
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
            if (theQueue.peek.asInstanceOf[Comparable[T]].compareTo(elem) < 0)            
                downTop(elem)
            else 
                false
        } else 
            theQueue.add(elem)
    }
    
    def +=(elem: T) = add(elem)
    
    def length() = theQueue.size
    
    def size() = theQueue.size
    
    def toArray() = theQueue.toArray.map(_.asInstanceOf[T])
    
    /**
     * move elements in this bounded priority queue to another bounded priority queue
     * note: this priority queue will be empty
     */
    def moveTo(b: BoundedPriorityQueue[T]):BoundedPriorityQueue[T] = {
        if (size < b.size) {
            for (i <- elements)
                b.add(i.asInstanceOf[T])
            theQueue.clear()
        } else {
            b.moveTo(this)
            val tmp = theQueue
            theQueue = b.theQueue
            b.theQueue = tmp
        }
        b
    }
    
    def ->(b: BoundedPriorityQueue[T]) = moveTo(b)
}