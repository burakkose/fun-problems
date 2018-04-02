package net.koseburak.fun

import scala.annotation.tailrec

object KMostFrequentWord {
  sealed trait MinHeapOperation {
    def extractMin: Option[MinHeapNode]
    def insert(word: String): Unit
    def collect: Seq[MinHeapNode]
    protected def swap(i: Int, j: Int): Unit
    protected def buildMinHeap(): Unit
    protected def minHeapify(i: Int): Unit
    protected def arr: Array[MinHeapNode]
    protected def parent(i: Int): Int = (i - 1) / 2
    protected def left(i: Int): Int = 2 * i + 1
    protected def right(i: Int): Int = 2 * i + 2
  }

  case class MinHeapNode(word: String, freq: Int = 1)
  case class MapEntry(freq: Int, idx: Int)

  case class MinHeap(capacity: Int) extends MinHeapOperation {
    import scala.collection.mutable

    private var count: Int = 0
    private val mapping = mutable.Map[String, MapEntry]()
    protected val arr: Array[MinHeapNode] = Array.ofDim[MinHeapNode](capacity)

    def collect: List[MinHeapNode] = {
      @tailrec
      def inner(maybeNode: Option[MinHeapNode],
                acc: List[MinHeapNode]): List[MinHeapNode] = {
        maybeNode match {
          case Some(node) => inner(extractMin, node :: acc)
          case _          => acc
        }
      }
      inner(extractMin, List.empty[MinHeapNode])
    }

    def extractMin: Option[MinHeapNode] = {
      if (count == 1) {
        val min = arr(0)
        count -= 1
        mapping.update(min.word, mapping(min.word).copy(idx = -1))
        Some(min)
      } else if (count > 1) {
        val min = arr(0)
        arr(0) = arr(count - 1)
        count -= 1
        mapping.update(min.word, mapping(min.word).copy(idx = -1))
        minHeapify(0)
        Some(min)
      } else None
    }

    def insert(word: String): Unit = {
      if (mapping.contains(word) && mapping(word).idx != -1) { // already in the minHeap
        val entry = mapping(word)
        val idx = entry.idx
        arr(idx) = arr(idx).copy(freq = entry.freq + 1)
        mapping(word) = entry.copy(freq = arr(idx).freq)
        minHeapify(idx)
      } else if (count < capacity) { // word is not present in the minheap and capacity is not full
        count += 1
        val i = count - 1
        arr(i) = MinHeapNode(word)
        mapping(word) = mapping
          .get(word)
          .map(t => t.copy(freq = t.freq + 1, idx = i))
          .getOrElse(MapEntry(1, i))
        buildMinHeap()
      } else if (incFreqOrGetDefault(word) > arr(0).freq) { // word is not present and capacity is full
        mapping(arr(0).word) = mapping(arr(0).word).copy(idx = -1)
        arr(0) = arr(0).copy(word = word,
                             freq = incFreqOrGetDefault(word))
        mapping(word) = mapping
          .get(word)
          .map(t => t.copy(freq = t.freq + 1, idx = 0))
          .getOrElse(MapEntry(1, 0))
        minHeapify(0)
      } else mapping += (word -> MapEntry(1, -1))
    }

    protected def minHeapify(i: Int): Unit = {
      val l = left(i)
      val r = right(i)
      var smallest = i
      if (l < count && arr(l).freq < arr(i).freq)
        smallest = l
      if (r < count && arr(r).freq < arr(smallest).freq)
        smallest = r
      if (smallest != i) {
        swap(i, smallest)
        minHeapify(smallest)
      }
    }

    protected def buildMinHeap(): Unit = {
      val n = count - 1
      var i = (n - 1) / 2
      while (i >= 0) {
        minHeapify(i)
        i -= 1
      }
    }

    protected def swap(i: Int, j: Int): Unit = {
      mapping(arr(i).word) = mapping(arr(i).word).copy(idx = j)
      mapping(arr(j).word) = mapping(arr(j).word).copy(idx = i)
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    private def incFreqOrGetDefault(word: String): Int =
      mapping.get(word).map(_.freq + 1).getOrElse(1)

  }

  def findMostFrequentWords(text: List[String],
                            k: Int): Seq[MinHeapNode] = {
    val minHeap = MinHeap(k)
    text.foreach(t => minHeap.insert(t.trim))
    minHeap.collect
  }

  val text =
    """
      |Welcome to the world of Geeks
      | This portal has been created to provide well written well thought and well explained
      | solutions for selected questions If you like Geeks for Geeks and would like to contribute
      | here is your chance You can write article and mail your article to contribute at
      | geeksforgeeks org See your article appearing on the Geeks for Geeks main page and help
      | thousands of other Geeks
    """.stripMargin

  findMostFrequentWords(text.split(" ").toList, 5)
}
