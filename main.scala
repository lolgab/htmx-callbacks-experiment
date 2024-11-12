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
      onClick: Callback[T, ?]
  )(mods: Modifier*) =
    all.button(
      postModifiers(onClick),
      mods
    )

def basePage(content: Modifier*) = html(
  head(
    script(src := "https://unpkg.com/htmx.org@2.0.3"),
    script(src := "https://unpkg.com/htmx.org/dist/ext/json-enc.js")
  ),
  body(content)
)

object callbacks extends Callbacks:
  val counterButton =
    CallbackBuilder.envOnly(counter)
  val addTodo =
    CallbackBuilder[List[Todo], Todo]((env, input) => todolist(env :+ input))

def counter(env: Int): Frag =
  div(
    "Counter ",
    id := "counter",
    env,
    components.button(onClick = callbacks.counterButton(env + 1))(
      hx.target := "#counter",
      "+"
    ),
    components.button(onClick = callbacks.counterButton(env - 1))(
      hx.target := "#counter",
      "-"
    )
  )

case class Todo(text: String) derives ReadWriter

def todolist(todos: List[Todo]): Frag =
  form(
    hx.target := "#todo-list",
    postModifiers(callbacks.addTodo(todos)),
    "TODO list",
    id := "todo-list",
    input(placeholder := "What do you need to do?", name := "text"),
    todos.map(todo => p(todo.text)),
    button(tpe := "submit", "Submit")
  )

@main
def main: Unit =
  callbacks.initialize()
  val index = basePage(todolist(List(Todo("Sleep"))), counter(0))

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
            exchange.getResponseSender.send(index.render)
      }
    })
    .build()
    .start()
