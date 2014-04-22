package net.spraycookies

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
        val cookiedreq = cookiesource.foldLeft(req)((_, jar) ⇒ addCookies(jar)(req))
        val fresp = innerPipeline(cookiedreq)
        fresp.map(res ⇒ {
          cookietarget.foldLeft(res)((_, jar) ⇒ storeCookies(jar, req.uri)(res))
        })
      }
  }

  def addCookies(cookiejar: CookieJar): HttpRequest ⇒ HttpRequest = {
    req: HttpRequest ⇒
      {
        val cookies = cookiejar.cookiesfor(req.uri)
        if (cookies.isEmpty) req
        else {
          val cookieheader = Cookie(cookies.toList)
          RequestBuilding.addHeader(cookieheader)(req)
        }
      }
  }

  def storeCookies(cookiejar: CookieJar, uri: ⇒ Uri): HttpResponse ⇒ HttpResponse = {
    res: HttpResponse ⇒
      {
        val cookieHeaders = res.headers collect { case c: `Set-Cookie` ⇒ c }
        for (c ← cookieHeaders.map(ch ⇒ ch.cookie)) {
          cookiejar.setCookie(c, uri)
        }
        res
      }
  }
}

