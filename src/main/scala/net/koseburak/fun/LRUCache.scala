package net.koseburak.fun

sealed trait DoubleLinkedList {
  def prev: DoubleLinkedList
  def next: DoubleLinkedList
  def value: Int = throw new IllegalAccessException("Does not have any value")
}

case class Cons(override val value: Int,
                var prev: DoubleLinkedList = Nil,
                var next: DoubleLinkedList = Nil)
    extends DoubleLinkedList

case object Nil extends DoubleLinkedList {
  override def prev: DoubleLinkedList = Nil
  override def next: DoubleLinkedList = Nil
}

case class LRUCache(capacity: Int) {
  import scala.annotation.tailrec
  import scala.collection.mutable

  private var head: DoubleLinkedList = Nil
  private var last: DoubleLinkedList = Nil
  private val references: mutable.HashMap[Int, DoubleLinkedList] =
    mutable.HashMap[Int, DoubleLinkedList]()

  def refer(i: Int): Unit = {
    if (!references.contains(i)) {
      enqueue(i)
    } else {
      val page = references(i)
      moveToHead(page)
    }

    references += (i -> head)
  }

  def display: String = {
    @tailrec
    def inner(h: DoubleLinkedList, acc: String): String = {
      h match {
        case Nil              => acc
        case Cons(v, _, next) => inner(next, acc + " " + v)
      }
    }
    inner(head, "")
  }

  private def dequeue(): Unit = {
    last = last match {
      case Cons(v, prev: Cons, _) =>
        references -= v
        prev.next = Nil
        prev
      case _ => last
    }
  }

  private def enqueue(i: Int): Unit = {
    head = head match {
      case Nil =>
        last = Cons(i, Nil, Nil)
        last
      case n: Cons =>
        val size = references.size
        if (size >= capacity) {
          dequeue()
        }
        val temp = Cons(i, Nil, n)
        n.prev = temp
        temp
    }
  }

  private def moveToHead(c: DoubleLinkedList): Unit = {
    c match {
      case n @ Cons(_, prev: Cons, next) => // not head
        prev.next = next
        next match {
          case n: Cons => n.prev = prev
          case _       => ()
        }
        n.next = head
        head match {
          case h: Cons => h.prev = n
          case _       => ()
        }
        head = n
      case _ => ()
    }
  }
}

object LRUCache {
  val cache = LRUCache(3)
  List(1, 2, 3, 4, 1, 2, 5, 1, 2, 3, 4, 5)
    .foreach(cache.refer)
  cache.display
}
