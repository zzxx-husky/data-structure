
import scala.collection.mutable.PriorityQueue
import scala.io.Source
import scala.collection.immutable.Range.Inclusive

private class Node[T](v: T) {
    def value = v
    var son = null.asInstanceOf[Node[T]]
    var lastSon = null.asInstanceOf[Node[T]]
    var slibling = null.asInstanceOf[Node[T]]

    /**
     * For the root node, one is pointing to the first son, the other is to the last son
     */
    def union(b: Node[T]): Node[T] = {
        if (son == null) {
            son = b
        } else {
            lastSon.slibling = b
        }
        b.slibling = null
        lastSon = b
        this
    }
    
    def show(indent: Int = 2) {
        for (i <- 0 until indent) print(' ')
        println(value)
        if (son != null)
            son.show(indent + 2)
        if (slibling != null)
            slibling.show(indent)
    }
}

class FibonacciHeap[T](implicit order: Ordering[T]) {

    private val heaps = new Array[Node[T]](32)
    private var maxIdx = 0 //heap with index larger than or equal to maxIdx must be null 

    private var tmpNode = null.asInstanceOf[Node[T]]
    private var topNode = null.asInstanceOf[Node[T]]

    private def addNodeTo(node: Node[T]) = {
        if (node == null) {
            try { tmpNode } finally { tmpNode = null }
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
        var i = 0
        while (i <= maxIdx) { //while loop is faster than find
            heaps.update(i, addNodeTo(heaps(i)))
            if (tmpNode == null)
                i += maxIdx + 1
            else 
                i += 1
        }
        if (heaps(maxIdx) != null) maxIdx += 1
    }

    /**
     * merge two fibonacci heap, the second heap will become empty after being merged.
     * O(logN)
     */
    def merge(b: FibonacciHeap[T]) {
        if (topNode == null || (b.topNode != null && order.compare(topNode.value, b.topNode.value) > 0))
            topNode = b.topNode
        maxIdx = math.min(maxIdx, b.maxIdx)
        FibonacciHeap.seq(0)(maxIdx).foreach {
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
        FibonacciHeap.seq(maxIdx + 1)(b.maxIdx - 1).foreach(b.heaps(_) = null)
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
            var son = topNode.son
            if (son != null) {
                var i = 0
                while (i <= maxIdx) { //need only one while for all sons
                    if (son != null) {
                        val nxt = son.slibling
                        son.slibling = null
                        if (tmpNode != null) {
                            addNodeTo(son)                                
                        } else {
                            tmpNode = son
                            heaps.update(i, addNodeTo(heaps(i)))
                        }
                        son = nxt
                        if (son == null) {
                            heaps.update(i + 1, null)
                            if (tmpNode == null) i += maxIdx
                        }
                    } else {
                        heaps.update(i, addNodeTo(heaps(i)))
                        if (tmpNode == null) i += maxIdx
                    }
                    i += 1
                }
            } else heaps.update(0, null)
            topNode = null
            if (heaps(maxIdx) != null) maxIdx += 1
            FibonacciHeap.seq(0)(maxIdx).foreach {
                i =>
                    if (heaps(i) != null)
                        topNode = if (topNode == null || order.compare(heaps(i).value, topNode.value) < 0) heaps(i) else topNode 
            }
            res
        }
    }

    def isEmpty() = topNode == null

    def nonEmpty() = topNode != null
    
    /**
     * for debug
     */
    def show() = FibonacciHeap.seq(0)(31).filter(heaps(_) != null).foreach{
        i =>
            println(i + ": ")
            heaps(i).show()
    }
}

object FibonacciHeap {
    //to get a sequence from i to j quickly
    private val seq = (0 until 32).map { i => (0 until 32).map(j => (i to j)) }
    
    def main(args: Array[String]) {
        val heap = new FibonacciHeap[Int]
        val a = (1 to 2000000).map(_ => (math.random * 1000).toInt % 1000).toArray
//        val sortedA = a.sorted
//        a.foreach(i => heap.add(i))
//        var idx = 0
//        while (heap.nonEmpty()) {
//            val i = heap.pop()
//            assert(sortedA(idx) == i, sortedA(idx) + " " + i)
//            idx += 1
//        }
//        println("SUCC")

        var start = System.currentTimeMillis()
        a.foreach{i => heap.add(i)}
        println(System.currentTimeMillis() - start)
        while (heap.nonEmpty) {
            heap.pop()
        }
        println(System.currentTimeMillis() - start)
        
        var st = System.currentTimeMillis()
        val Q = new PriorityQueue[Int]()
        a.foreach(i => Q += i)
        println(System.currentTimeMillis() - st)
        while (Q.nonEmpty)
            Q.dequeue()
        println(System.currentTimeMillis() - st)
    }
}