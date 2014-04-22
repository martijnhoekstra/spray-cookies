package net.spraycookies.tldlist

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object EffectiveTldListSpecification extends Properties("EffectiveTldListSpecification") {

  property("contains") = forAll { (a: String, b: String) ⇒
    val tldlist = new TrieTldList {
      val domaintrie = TldTrie(List(a).iterator)
    }

    tldlist.contains(a) && (b == "" || !tldlist.contains(b + a))
  }
}
