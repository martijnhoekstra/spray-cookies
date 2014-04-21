package spray.cookies

import spray.http.HttpHeaders.Host
import spray.http.Uri
import scala.annotation.tailrec
import spray.http.DateTime
import spray.http.HttpCookie
import spray.http.HttpResponse

class CookieJar(blacklist: EffectiveTldList) {

  private case class StoredCookie(name: String, content: String, expires: Option[DateTime], domain: String, path: String, httpOnly: Boolean, secure: Boolean)

  private object StoredCookie {
    implicit def toHttpCookie(src: StoredCookie) = {
      HttpCookie(src.name, src.content, src.expires: Option[DateTime], None, Some(src.domain), Some(src.path), src.secure, src.httpOnly, None)
    }
    implicit def toStoredCookie(src: HttpCookie)(implicit uri: Uri) = {
      val domain = src.domain.getOrElse(uri.authority.host.address)
      val path = src.path.getOrElse(uri.path.toString)
      val expiration = src.expires match {
        case x: Some[DateTime] ⇒ x
        case None              ⇒ src.maxAge.map(age ⇒ DateTime.now + age)
      }
      StoredCookie(src.name, src.content, expiration, domain, path, src.httpOnly, src.secure)
    }
  }

  private var jar: CookieJar_ = CookieJar_("", Map.empty, Map.empty)

  def cookiesfor(uri: Uri) = jar.cookiesfor(uri).map(c ⇒ StoredCookie.toHttpCookie(c))

  def setCookie(cookie: HttpCookie, source: Uri) = {
    val storedcookie = StoredCookie.toStoredCookie(cookie)(source)
    if (isAllowedFor(storedcookie, source)) {
      jar = jar.setCookie(storedcookie)
      true
    } else false
  }

  private def isAllowedFor(cookie: StoredCookie, source: Uri): Boolean = {
    !blacklist.contains(cookie.domain) &&
      isDomainPostfix(cookie.domain, source.authority.host.address)
    //status of cookie paths is not clear to me, not implemented
  }

  private def isDomainPostfix(needle: String, haystack: String) = {
    val needleelements = needle.split('.').toList.reverse
    val haystackelements = haystack.split('.').toList.reverse
    haystackelements.startsWith(needleelements)

  }

  private case class CookieJar_(domainElement: String, subdomains: Map[String, CookieJar_], cookies: Map[String, StoredCookie]) {
    def cookiesfor(uri: Uri) = {
      val domain = uri.authority.host.address
      val domainelements = domain.split('.').toList.reverse
      _getCookies(domainelements, uri, Map.empty).values
    }

    @tailrec
    private def _getCookies(domain: List[String], uri: Uri, accum: Map[String, StoredCookie]): Map[String, StoredCookie] = {
      val now = DateTime.now
      val newcookies = removeStale(cookies, now)
        .filter(c ⇒ uri.scheme == "https" || !c._2.secure)
        .filter(c ⇒ uri.path.startsWith(Uri.Path(c._2.path)))
      val totalCookies = accum ++ newcookies
      domain match {
        case Nil ⇒ totalCookies
        case head :: tail ⇒ {
          subdomains.get(head) match {
            case None      ⇒ totalCookies
            case Some(jar) ⇒ jar._getCookies(tail, uri, totalCookies)
          }
        }
      }
    }

    def setCookie(cookie: StoredCookie) = {
      val trimmed = if (cookie.domain.indexOf('.') == 0) cookie.domain.substring(1) else cookie.domain
      val domainelements = trimmed.split('.').toList.reverse
      _setCookie(domainelements, cookie)
    }

    private def _setCookie(domain: List[String], cookie: StoredCookie): CookieJar_ = {
      val now = DateTime.now
      domain match {
        case Nil ⇒ {
          val newcookies = removeStale(cookies, now) + (cookie.name -> cookie)
          this.copy(cookies = newcookies)
        }
        case head :: tail ⇒ {
          lazy val newsubjar = CookieJar_(head, Map.empty, Map.empty)
          val subjar = subdomains.getOrElse(head, newsubjar)
          this.copy(subdomains = subdomains + (head -> subjar._setCookie(tail, cookie)))
        }
      }
    }

    def removeStale(cookies: Map[String, StoredCookie], cutoff: DateTime) =
      cookies.filter(c ⇒ c._2.expires.map(_ > cutoff).getOrElse(true))
  }
}