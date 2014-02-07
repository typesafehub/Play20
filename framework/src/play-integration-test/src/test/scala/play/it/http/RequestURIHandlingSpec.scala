package play.it.http

import play.api.mvc._
import play.api.mvc.Results._
import play.api.test._
import play.api.libs.ws.Response

object RequestURIHandlingSpec extends PlaySpecification {

  "Request-URI handling" should {

    // See: http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html

    def makeRequest[T](path: String)(block: Response => T) = withServer { implicit port =>
      val response = await(wsUrl(path).get())
      block(response)
    }

    def withServer[T](block: Port => T) = {
      val port = testServerPort
      running(TestServer(port, FakeApplication(
        withRoutes = {
          case _ => Action { request => Ok(request.path) }
        }
      ))) {
        block(port)
      }
    }

    "handle a Request-URI of /" in makeRequest("/") { response =>
      response.body must_== "/"
    }
    "handle a Request-URI of /x" in makeRequest("/x") { response =>
      response.body must_== "/x"
    }
    "handle a Request-URI of /x/y" in makeRequest("/x/y") { response =>
      response.body must_== "/x/y"
    }
    "handle a Request-URI of /a-b/cde/fgh?ij=XY" in makeRequest("/a-b/cde/fgh?ij=XY") { response =>
      response.body must_== "/a-b/cde/fgh"
    }
    "handle a Request-URI of /abc_def_gh?ij=XY" in makeRequest("/abc_def_gh?ij=XY") { response =>
      response.body must_== "/abc_def_gh"
    }

    "handle a Request-URI of //" in makeRequest("//") { response =>
      response.body must_== "//"
    }
    "handle a Request-URI of //x" in makeRequest("//x") { response =>
      response.body must_== "//x"
    }
    "handle a Request-URI of //x/y" in makeRequest("//x/y") { response =>
      response.body must_== "//x/y"
    }
    "handle a Request-URI of //x/y" in makeRequest("//x/y") { response =>
      response.body must_== "//x/y"
    }
    "handle a Request-URI of //a-b/cde/fgh?ij=XY" in makeRequest("//a-b/cde/fgh?ij=XY") { response =>
      response.body must_== "//a-b/cde/fgh"
    }
    "handle a Request-URI of //abc_def_gh?ij=XY" in makeRequest("//abc_def_gh?ij=XY") { response =>
      response.body must_== "//abc_def_gh"
    }

  }

}
