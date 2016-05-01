
import scala.collection.mutable.PriorityQueue
import scala.io.Source
import scala.collection.immutable.Range.Inclusive
import scala.reflect.ClassTag

private case class Node[T](value: T) {
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
    
    def recursive[U](p: Node[T] => U): U = p(this)
}

class FibonacciHeap[T](implicit order: Ordering[T]) {

    private var heaps = new Array[Node[T]](32)
    private var maxIdx = 0 //heap with index larger than or equal to maxIdx must be null 

    private var tmpNode = null.asInstanceOf[Node[T]]
    private var topNode = null.asInstanceOf[Node[T]]
    
    private var numElem = 0

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
        while (tmpNode != null) { //while loop is faster than find
            heaps.update(i, addNodeTo(heaps(i)))
            i += 1
        }
        numElem += 1
        if (heaps(maxIdx) != null) maxIdx += 1
    }
    
    def clear() {
        topNode = null
        maxIdx = 0
        numElem = 0
        FibonacciHeap.seq(0)(31).foreach(heaps.update(_, null))
    }

    /**
     * merge two fibonacci heap, the second heap will become empty after being merged.
     * O(logN)
     */
    def <=(b: FibonacciHeap[T]): FibonacciHeap[T] = {
        if (topNode == null || (b.topNode != null && order.compare(topNode.value, b.topNode.value) > 0))
            topNode = b.topNode
        if (maxIdx < b.maxIdx) {
            val tmpIdx = b.maxIdx
            b.maxIdx = maxIdx
            maxIdx = tmpIdx
            val tmpHeaps = b.heaps
            b.heaps = heaps
            heaps = tmpHeaps
        }
        var i = 0
        while (i < b.maxIdx) {
            if (tmpNode == null) {
                tmpNode = b.heaps(i)
            }
            if (tmpNode != null) {
                if (b.heaps(i) != null)
                    addNodeTo(b.heaps(i))
                else
                    heaps.update(i, heaps(i))
            }
            i += 1
        }
        while (tmpNode != null) {
            heaps.update(i, addNodeTo(heaps(i)))
            i += 1
        }
        numElem += b.numElem
        b.clear()
        if (heaps(maxIdx) != null) maxIdx += 1
        this
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
                        if (nxt == null) {
                            heaps.update(i + 1, null)
                            if (tmpNode == null) i += maxIdx
                        }
                        son = nxt
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
            numElem -= 1
            res
        }
    }
    
    def toArray()(implicit tag: ClassTag[T]) = {
        val res = new Array[T](numElem)
        var idx = 0
        def recu: Node[T] => Unit = {
            node =>
                res.update(idx, node.value)
                idx += 1
                if (node.son != null)
                    node.son.recursive(recu)
                if (node.slibling != null)
                    node.slibling.recursive(recu)
        }
        heaps.filter(_ != null).foreach(_.recursive(recu))
        res
    }

    def isEmpty() = topNode == null

    def nonEmpty() = topNode != null

    /**
     * for debug
     */
    def show() = {
        def toShow: (Node[T], Int) => Unit = {
            case (node, indent) =>
                for (i <- 0 until indent) print(' ')
                println(node.value)
                if (node.son != null)
                    toShow(node.son, indent + 2)
                if (node.slibling != null)
                    toShow(node.slibling, indent)
        }
        FibonacciHeap.seq(0)(31).filter(heaps(_) != null).foreach {
            i =>
                println(i + ": ")
                heaps(i).recursive(toShow(_, 2))
        }
    }
}

object FibonacciHeap {
    //to get a sequence from i to j quickly
    private val seq = (0 until 32).map { i => (0 until 32).map(j => (i to j)) }  
    
    def empty[T](implicit order: Ordering[T]) = new FibonacciHeap[T]()(order)

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
        a.foreach { i => heap.add(i) }
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