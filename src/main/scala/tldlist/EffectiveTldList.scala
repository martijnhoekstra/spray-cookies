package net.spraycookies.tldlist

import scala.io.Source

trait EffectiveTldList {
  def contains(domain: String): Boolean
}

trait TrieTldList extends EffectiveTldList {
  def domaintrie: TldTrie

  private def contains(domain: List[String]) = domaintrie.contains(domain)
  def contains(domain: String) = domaintrie.contains(domain.split('.').reverse.toList)

}

object DefaultEffectiveTldList extends TrieTldList {
  private val lines = {
    val instream = getClass().getResourceAsStream("/effectivetlds.lst")
    Source.fromInputStream(instream, "UTF-8").getLines.filterNot(_.startsWith("//")).filterNot(_.isEmpty)
  }

  val domaintrie = TldTrie(lines)

}