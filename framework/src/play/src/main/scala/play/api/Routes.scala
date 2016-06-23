/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api {

  import play.api.templates.JavaScript

  /**
   * Helper utilities related to `Router`.
   */
  object Routes {

    // -- TAGS

    val ROUTE_VERB = "ROUTE_VERB"
    val ROUTE_PATTERN = "ROUTE_PATTERN"
    val ROUTE_CONTROLLER = "ROUTE_CONTROLLER"
    val ROUTE_ACTION_METHOD = "ROUTE_ACTION_METHOD"
    val ROUTE_COMMENTS = "ROUTE_COMMENTS"

    // --

    import play.core.Router._
    import play.api.mvc.RequestHeader

    /**
     * Generates a JavaScript router.
     *
     * For example:
     * {{{
     * Routes.javascriptRouter("MyRouter")(
     *   controllers.routes.javascript.Application.index,
     *   controllers.routes.javascript.Application.list,
     *   controllers.routes.javascript.Application.create
     * )
     * }}}
     *
     * And then you can use the JavaScript router as:
     * {{{
     * var routeToHome = MyRouter.controllers.Application.index()
     * }}}
     *
     * @param name the JavaScript object name
     * @param routes the routes to include in this JavaScript router
     * @return the JavaScript code
     */
    def javascriptRouter(name: String = "Router", ajaxMethod: Option[String] = Some("jQuery.ajax"))(routes: JavascriptReverseRoute*)(implicit request: RequestHeader): JavaScript = {
      javascriptRouter(name, ajaxMethod, request.host, routes: _*)
    }

    val jsReservedWords = Seq("break", "case", "catch", "continue", "debugger",
      "default", "delete", "do", "else", "finally", "for", "function", "if",
      "in", "instanceof", "new", "return", "switch", "this", "throw", "try",
      "typeof", "var", "void", "while", "with")

    // TODO: This JS needs to be re-written as it isn't easily maintained.
    def javascriptRouter(name: String, ajaxMethod: Option[String], host: String, routes: JavascriptReverseRoute*): JavaScript = JavaScript {
      import org.apache.commons.lang3.StringEscapeUtils.{ escapeEcmaScript => esc }
      val ajaxField = ajaxMethod.fold("")(m => s"ajax:function(c){c=c||{};c.url=r.url;c.type=r.method;return $m(c)},")
      val routesStr = routes.map { route =>
        val nameParts = route.name.split('.')
        val controllerName = nameParts.dropRight(1).mkString(".")
        val prop = "_root" + nameParts.map(p => s"['${esc(p)}']").mkString
        s"_nS('${esc(controllerName)}'); $prop = ${route.f};"
      }.mkString("\n")
      s"""
         |var $name = {}; (function(_root){
         |var _nS = function(c,f,b){var e=c.split(f||"."),g=b||_root,d,a;for(d=0,a=e.length;d<a;d++){g=g[e[d]]=g[e[d]]||{}}return g}
         |var _qS = function(items){var qs = ''; for(var i=0;i<items.length;i++) {if(items[i]) qs += (qs ? '&' : '') + items[i]}; return qs ? ('?' + qs) : ''}
         |var _s = function(p,s){return p+((s===true||(s&&s.secure))?'s':'')+'://'}
         |var _wA = function(r){return {$ajaxField method:r.method,type:r.method,url:r.url,absoluteURL: function(s){return _s('http',s)+'${esc(host)}'+r.url},webSocketURL: function(s){return _s('ws',s)+'${esc(host)}'+r.url}}}
         |$routesStr
         |})($name)
    """.stripMargin.trim
    }

  }

}
