case class Node[T](value: T) {
    var numSon = 0
    var son = null.asInstanceOf[Node[T]]
    var slibing = null.asInstanceOf[Node[T]]
        
    def union(b: Node[T]): Node[T] = {
        b.slibing = son
        son = b
        numSon += 1
        this
    }
}

class FibonacciHeap[T](implicit order: Ordering[T]) {
    private val heaps = new Array[Node[T]](32)
    private var maxIdx = 1 //heap with index larger than or equal to maxIdx must be null 
    
    private var minNode = null.asInstanceOf[Node[T]]
    
    private def addNodeTo(i: Int) = {
        if (heaps(i) == null) {
            heaps(i) = heaps(0)
            true
        } else {
            val ord = order.compare(heaps(i).value, heaps(0).value)
            heaps(0) = if (ord < 0) heaps(i).union(heaps(0)) else heaps(0).union(heaps(i))
            heaps(i) = null
            false
        }
    }

    def min(): Option[T] = if (minNode == null) None else Some(minNode.value)
    
    def add(value: T) {
        heaps(0) = new Node(value)
        if (minNode == null || order.compare(minNode.value, value) > 0)
            minNode = heaps(0)
        (1 to maxIdx).toStream.find(i => addNodeTo(i))
        if (heaps(maxIdx) != null) maxIdx += 1
        heaps(0) = null
    }
    
    /**
     * merge two fibonacci heap, the second heap will become empty after being merged.
     */
    def merge(b: FibonacciHeap[T]) {
        if (minNode == null || (b.minNode != null && order.compare(minNode.value, b.minNode.value) > 0))
            minNode = b.minNode
        b.minNode = null
        maxIdx = math.max(maxIdx, b.maxIdx)
        (1 to maxIdx).toStream.foreach {
            i => 
                b.heaps(i) = 
                    if (heaps(i) != null && b.heaps(i) != null) {
                        val ord = order.compare(heaps(i).value, b.heaps(i).value)
                        if (ord < 0) heaps(i).union(b.heaps(i)) else b.heaps(i).union(heaps(i))
                    } else if (heaps(i) != null) {
                        heaps(i)
                    } else b.heaps(i)
                heaps(i) = heaps(0)
                heaps(0) = b.heaps(i)
                b.heaps(i) = null
        }
        if (heaps(maxIdx) != null) maxIdx += 1
    }
    
    def removeMin(): Option[T] = {
        if (minNode == null) None
        else {
            val res = Some(minNode.value)
            heaps(minNode.numSon) = null
            while(minNode.son != null) {
                heaps(0) = minNode.son
                (minNode.son.numSon to maxIdx).toStream.find(i => addNodeTo(i))
                if (heaps(maxIdx) != null) maxIdx += 1
                minNode.son = minNode.son.slibing
            }
            heaps(0) = null
            minNode = (1 to maxIdx).map(heaps(_)).min(new Ordering[Node[T]] {
                override def compare(a: Node[T], b: Node[T]) = if (a == null) 1 else if (b == null) -1 else order.compare(a.value, b.value)
            })
            res
        }
    }
    
    def isEmpty() = minNode == null
    
    def nonEmpty() = minNode != null
}

object FibonacciHeap {
    def main(args: Array[String]) {
        val heap = new FibonacciHeap[Int]
        val array = (1 to 100).map(_ => (math.random * 1000).toInt % 1000)
        println(array.sorted.mkString(" "))
        array.foreach(i => heap.add(i))
        while(heap.nonEmpty()) print(heap.removeMin() + " ")
    }
}