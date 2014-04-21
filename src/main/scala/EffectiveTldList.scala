package spray.cookies

import scala.io.Source

trait EffectiveTldList {
  def contains(domain: String): Boolean
}

object DefaultEffectiveTldList extends EffectiveTldList {
  private val lines = {
    val instream = getClass().getResourceAsStream("/effectivetlds.lst")
    Source.fromInputStream(instream, "UTF-8").getLines.filterNot(_.startsWith("//")).filterNot(_.isEmpty)
  }

  private val list = TldTrie(lines)

  private def contains(domain: List[String]) = list.contains(domain)
  def contains(domain: String) = list.contains(domain.split('.').reverse.toList)

  private sealed trait TldTrie {
    def contains(list: List[String]): Boolean = {
      list match {
        case Nil ⇒ true
        case head :: tail ⇒ this match {
          case Leaf                ⇒ false
          case Wildcard(negations) ⇒ tail == Nil && !negations.contains(head)
          case Node(m) ⇒ m.get(head) match {
            case None           ⇒ false
            case Some(trietail) ⇒ trietail.contains(tail)
          }
        }
      }
    }
    def merge(that: TldTrie): TldTrie
  }

  private case class Node(map: Map[String, TldTrie]) extends TldTrie {
    def merge(that: TldTrie): TldTrie = {
      that match {
        case Node(thatmap) ⇒ Node(TldTrie.mapmerge(map, thatmap, (_: TldTrie).merge(_: TldTrie)))
        case Leaf          ⇒ this
        case x: Wildcard   ⇒ throw new Exception(s"tries $x and $this not mergable")
      }
    }
  }

  private case object Leaf extends TldTrie {
    def merge(that: TldTrie) = that
  }

  private case class Wildcard(whitelist: Set[String]) extends TldTrie {
    def merge(that: TldTrie) = that match {
      case Leaf                    ⇒ this
      case Wildcard(thatwhitelist) ⇒ Wildcard(whitelist ++ thatwhitelist)
      case x: Node                 ⇒ throw new Exception(s"tries $x and $this not mergable")
    }
  }

  private object TldTrie {

    def mapmerge[T, U](left: Map[T, U], right: Map[T, U], merge: (U, U) ⇒ U): Map[T, U] = {
      left.foldLeft(right)((nm, kvp) ⇒ {
        nm.get(kvp._1) match {
          case None       ⇒ nm + kvp
          case Some(val2) ⇒ nm + (kvp._1 -> merge(val2, kvp._2))
        }
      })
    }

    private def totrie(elems: List[String]): TldTrie = {
      elems match {
        case head :: tail ⇒ {
          if (head == "*") Wildcard(Set.empty)
          else if (head.startsWith("!")) Wildcard(Set(head.substring(1)))
          else Node(Map(head -> totrie(tail)))
        }
        case Nil ⇒ Leaf
      }
    }

    def apply(domains: Iterator[String]): TldTrie = {
      domains.foldLeft(Leaf: TldTrie)((r, domain) ⇒ {
        val elems = domain.split('.').toList.reverse
        r.merge(totrie(elems))
      })
    }
  }
}
