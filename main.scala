package app

import library.*
import scalatags.Text.all.*
import _root_.io.undertow.*
import _root_.io.undertow.server.*
import _root_.io.undertow.util.*
import upickle.default.*

object components:
  import scalatags.Text.all
  def button[T: ReadWriter](
      onClick: Callback[T]
  )(mods: Modifier*) =
    all.button(
      postModifiers(onClick),
      mods
    )

def basePage(content: Modifier) = html(
  head(
    script(src := "https://unpkg.com/htmx.org@2.0.3"),
    script(src := "https://unpkg.com/htmx.org/dist/ext/json-enc.js")
  ),
  body(content)
)

object callbacks extends Callbacks:
  object counterButton
      extends CallbackBuilder[Int]((value: Int) => counter(value))

def counter(value: Int): Frag =
  div(
    "Counter ",
    id := "counter",
    value,
    components.button(onClick = callbacks.counterButton(value + 1))(
      hx.target := "#counter",
      "+"
    ),
    components.button(onClick = callbacks.counterButton(value - 1))(
      hx.target := "#counter",
      "-"
    )
  )

@main
def main: Unit =
  callbacks.initialize()

  Undertow.builder
    .addHttpListener(8080, "127.0.0.1")
    .setHandler(new HttpHandler() {
      def handleRequest(exchange: HttpServerExchange): Unit = {
        if (exchange.isInIoThread()) {
          exchange.dispatch(this);
          return;
        }

        exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "text/html")

        Callbacks.getCallback(exchange.getRequestPath()) match
          case Some(callback) =>
            exchange.startBlocking()
            val isHTMXRequest =
              exchange.getRequestHeaders().get("HX-Request") != null
            val result = callback.run(exchange.getInputStream())
            val content = if (isHTMXRequest) result else basePage(result)
            exchange.getResponseSender.send(content.render)
          case None =>
            exchange.getResponseSender.send(basePage(counter(1)).render)
      }
    })
    .build()
    .start()
