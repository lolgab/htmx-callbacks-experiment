package library

import io.undertow.Undertow
import io.undertow.util.Headers

import scalatags.Text.all.*
import upickle.default.*
import io.undertow.server.*

import scala.collection.mutable

object hx:
  val get = attr("hx-get")
  val post = attr("hx-post")
  val vals = attr("hx-vals")
  val ext = attr("hx-ext")
  val target = attr("hx-target")

type Handler[T] = T => Frag

trait Callbacks:
  inline def initialize(): Unit =
    ${ macros.initializeMacroImpl[this.type] }

object Callbacks:
  private[library] val handlers = mutable.Map[String, CallbackBuilder[?]]()
  private[library] val paths = mutable.Map[CallbackBuilder[?], String]()

  def getCallback(path: String) = handlers.get(path)

def basePage(content: Modifier) = html(
  head(
    script(src := "https://unpkg.com/htmx.org@2.0.3")
  ),
  body(content)
)

final case class Callback[T: ReadWriter](
    callback: CallbackBuilder[T],
    data: T
)

def postModifiers[T: ReadWriter](cb: Callback[T]) = Seq(
  hx.post := cb.callback.path,
  hx.vals := write(ValueWrapper(cb.data)),
  hx.ext := "json-enc"
)

trait CallbackBuilder[T: ReadWriter](render: (t: T) => Frag):
  def apply(t: T): Callback[T] = Callback(this, t)

  def path: String =
    Callbacks.paths.getOrElse(
      this,
      sys.error(
        s"Can't find callback. Make sure to call `initialize()` and to define it as a static object."
      )
    )

  def run(inputStream: java.io.InputStream): Frag =
    render(read[ValueWrapper[T]](inputStream).value)

private case class ValueWrapper[T](value: T) derives ReadWriter
