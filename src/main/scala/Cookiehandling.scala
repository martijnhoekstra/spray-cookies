package spray.cookies

import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.HttpHeaders.`Set-Cookie`
import spray.http.HttpHeaders.Cookie
import spray.http.Uri
import scala.concurrent.Future
import spray.httpx.RequestBuilding
import scala.concurrent.ExecutionContext

object Cookiehandling {

  def withCookies(cookiesource: Option[CookieJar], cookietarget: Option[CookieJar])(innerPipeline: HttpRequest ⇒ Future[HttpResponse])(implicit context: ExecutionContext) = {
    req: HttpRequest ⇒
      {
        val cookiedreq = addCookies(cookiesource)(req)
        val fresp = innerPipeline(cookiedreq)
        fresp.map(res ⇒ {
          storeCookies(cookietarget, req.uri)(res)
        })
      }
  }

  def addCookies(cookiejar: Option[CookieJar]) = {
    req: HttpRequest ⇒
      {
        val cookies = cookiejar.toIterable.flatMap(jar ⇒ jar.cookiesfor(req.uri))
        if (cookies.isEmpty) req
        else {
          val cookieheader = Cookie(cookies.toList)
          RequestBuilding.addHeader(cookieheader)(req)
        }
      }
  }

  def storeCookies(cookiejar: Option[CookieJar], uri: Uri) = {
    res: HttpResponse ⇒
      {
        val cookieHeaders = res.headers collect { case c: `Set-Cookie` ⇒ c }
        for (c ← cookieHeaders.map(ch ⇒ ch.cookie)) {
          cookiejar.map(_.setCookie(c, uri))
        }
        res
      }
  }
}

