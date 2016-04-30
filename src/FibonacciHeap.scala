
import scala.collection.mutable.PriorityQueue
import scala.io.Source

private class Node[T](v: T) {
    def value = v
    var index = 0
    var son = null.asInstanceOf[Node[T]]
    var slibling = null.asInstanceOf[Node[T]]

    def union(b: Node[T]): Node[T] = {
        b.slibling = son
        index += 1
        son = b
        this
    }
}    

class FibonacciHeap[T](implicit order: Ordering[T]) {

    private val heaps = new Array[Node[T]](32)
    private var maxIdx = 0 //heap with index larger than or equal to maxIdx must be null 

    private var tmpNode = null.asInstanceOf[Node[T]]
    private var topNode = null.asInstanceOf[Node[T]]

    private def addNodeTo(node: Node[T]) = {
        if (node == null) {
            tmpNode
        } else {
            val ord = order.compare(node.value, tmpNode.value)
            tmpNode = if (ord < 0) node.union(tmpNode) else tmpNode.union(node)
            null
        }
    }

    /**
     * O(1)
     */
    def top(): T = if (topNode == null) throw new Exception("top from empty heap") else topNode.value

    /**
     * O(1)
     */
    def add(value: T) {
        tmpNode = new Node(value)
        if (topNode == null || order.compare(topNode.value, value) >= 0) //need equal to 0
            topNode = tmpNode
            (0 to maxIdx).find{
                i =>
                heaps(i) = addNodeTo(heaps(i))
                heaps(i) != null
            }
        if (heaps(maxIdx) != null) maxIdx += 1
        tmpNode = null
    }

    /**
     * merge two fibonacci heap, the second heap will become empty after being merged.
     * O(logN)
     */
    def merge(b: FibonacciHeap[T]) {
        if (topNode == null || (b.topNode != null && order.compare(topNode.value, b.topNode.value) > 0))
            topNode = b.topNode
        maxIdx = math.min(maxIdx, b.maxIdx)
        (0 to maxIdx).foreach {
            i =>
                b.heaps(i) =
                    if (heaps(i) != null && b.heaps(i) != null) {
                        val ord = order.compare(heaps(i).value, b.heaps(i).value)
                        if (ord < 0) heaps(i).union(b.heaps(i)) else b.heaps(i).union(heaps(i))
                    } else 
                        if (heaps(i) != null) heaps(i) else b.heaps(i)
                heaps(i) = tmpNode
                tmpNode = b.heaps(i)
                b.heaps(i) = null
        }
        (maxIdx + 1 until b.maxIdx).foreach(b.heaps(_) = null)
        b.topNode = null
        b.maxIdx = 0
        if (heaps(maxIdx) != null) maxIdx += 1
    }

    /**
     * O(logN)
     */
    def pop(): T = {
        if (topNode == null) throw new Exception("top from empty heap")
        else {
            val res = topNode.value
            heaps(topNode.index) = null
            var son = topNode.son
            while (son != null) {
                tmpNode = son
                son = son.slibling
                tmpNode.slibling = null
                (tmpNode.index to maxIdx).find{
                    i =>
                    heaps(i) = addNodeTo(heaps(i))
                    heaps(i) != null
                }
            }
            tmpNode = null
            topNode = null
            if (heaps(maxIdx) != null) maxIdx += 1
            for (i <- 0 to maxIdx) {
                if (heaps(i) != null)
                    topNode = if (topNode == null || order.compare(heaps(i).value, topNode.value) < 0) heaps(i) else topNode 
            }
            res
        }
    }

    def isEmpty() = topNode == null

    def nonEmpty() = topNode != null
}

object FibonacciHeap {
    def main(args: Array[String]) {
        val heap = new FibonacciHeap[Int]
        val a = (1 to 2000000).map(_ => (math.random * 1000).toInt % 1000)
        val sortedA = a.sorted
        a.foreach(i => heap.add(i))
        var idx = 0
        while (heap.nonEmpty()) {
            val i = heap.pop()
            assert(sortedA(idx) == i, sortedA(idx) + " " + i)
            idx += 1
        }
        println("SUCC")

        var st = System.currentTimeMillis()
        a.foreach(i => heap.add(i))
        println(System.currentTimeMillis() - st)
        while (heap.nonEmpty)
            heap.pop()
        println(System.currentTimeMillis() - st)
        
        st = System.currentTimeMillis()
        val Q = new PriorityQueue[Int]()
        a.foreach(i => Q += i)
        println(System.currentTimeMillis() - st)
        while (Q.nonEmpty)
            Q.dequeue()
        println(System.currentTimeMillis() - st)
    }
}