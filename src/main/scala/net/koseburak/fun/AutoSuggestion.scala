package net.koseburak.fun

object AutoSuggestion {

  import scala.annotation.tailrec
  import scala.collection.mutable.HashMap
  case class Trie(private var isWordEnd: Boolean = false,
                  private val children: HashMap[Char, Trie] = HashMap.empty) {
    def insert(key: String): Unit = {
      var temp = this
      key.foreach { ch =>
        if (!temp.children.contains(ch))
          temp.children += (ch -> Trie())
        temp = temp.children(ch)
      }
      temp.isWordEnd = true
    }

    def isLastNode: Boolean = children.isEmpty

    def suggest(query: String): List[String] = {
      @tailrec
      def findNode(i: Int, n: Trie): Option[Trie] = {
        if (i >= query.length) Some(n)
        else if (!n.children.contains(query.charAt(i))) None
        else findNode(i + 1, n.children(query.charAt(i)))
      }
      def suggestRecursive(node: Trie, prefix: String): List[String] = {
        if (node.isWordEnd && node.isLastNode)
          List(prefix)
        else {
          val res = node.children.toList.flatMap {
            case (k, v) =>
              suggestRecursive(v, prefix + k)
          }
          if (node.isWordEnd) prefix :: res else res
        }
      }
      Option(query)
        .filter(_.length > 2)
        .flatMap { _ =>
          val maybeNode = findNode(0, this)
          maybeNode.map { node =>
            val isWord = node.isWordEnd
            val isLastNode = node.isLastNode
            if (isLastNode && isWord)
              List(query)
            else if (!isLastNode)
              suggestRecursive(node, query)
            else List()
          }
        }
        .getOrElse(List.empty)
    }
  }

  val root = Trie()
  root.insert("hello")
  root.insert("dog")
  root.insert("hell")
  root.insert("cat")
  root.insert("a")
  root.insert("hel")
  root.insert("help")
  root.insert("helps")
  root.insert("helping")

  root.suggest("hl")

}
