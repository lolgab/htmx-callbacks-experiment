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
    ${ macros.initializeMacroImpl[this.type]('this) }

object Callbacks:
  private[library] val handlers = mutable.Map[String, CallbackBuilder[?, ?]]()
  private[library] val paths = mutable.Map[CallbackBuilder[?, ?], String]()

  def getCallback(path: String) = handlers.get(path)

def basePage(content: Modifier) = html(
  head(
    script(src := "https://unpkg.com/htmx.org@2.0.3")
  ),
  body(content)
)

final case class Callback[Env: ReadWriter, Input: ReadWriter](
    callback: CallbackBuilder[Env, Input],
    data: Env
)

private final val envKey = "_callbackEnvValue"

def postModifiers[T: ReadWriter](cb: Callback[T, ?]) = Seq(
  hx.post := cb.callback.path,
  hx.vals := write(ujson.Obj(envKey -> write(cb.data))),
  hx.ext := "json-enc"
)

case class WithInput[T](t: T, extraArgs: Map[String, String])

abstract class CallbackBuilder[Env: ReadWriter, Input: ReadWriter](
    render: (env: Env, input: Input) => Frag
):
  def this(render: (env: Env) => Frag) =
    this((env: Env, input: Input) => render(env))

  def apply(env: Env): Callback[Env, Input] = Callback(this, env)

  def path: String =
    Callbacks.paths.getOrElse(
      this,
      sys.error(
        s"Can't find callback. Make sure to call `initialize()` and to define it as a static object."
      )
    )

  def run(inputStream: java.io.InputStream): Frag =
    val json = read[ujson.Obj](inputStream)
    val t = read[Env](json(envKey).str)
    json.value.remove(envKey)
    val extraArgs = read[Input](json)
    render(t, extraArgs)
