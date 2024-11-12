package library
package macros

import scala.quoted.*

def initializeMacroImpl[T](
    callback: Expr[T]
)(using Type[T], Quotes): Expr[Unit] =
  import quotes.reflect.*

  def isStatic(symbol: Symbol): Boolean =
    symbol.flags.is(Flags.Package) ||
      symbol.flags.is(Flags.Final) && isStatic(symbol.owner)

  val symbol = TypeRepr.of[T].typeSymbol

  val callbackBuildersAndNames =
    symbol.declarations
      .filter(d =>
        d.isTerm &&
          d.typeRef.baseClasses
            .exists(c => c.fullName == "library.CallbackBuilder")
      )
      .map(d =>
        val prettyName = d.fullName.replace('.', '/')
        (callback.asTerm.select(d) -> Expr(prettyName))
      )

  Expr.block(
    callbackBuildersAndNames.map { case (callbackBuilder, name) =>
      val path = '{ s"/api/${${ name }}" }
      val cb = callbackBuilder.asExprOf[library.CallbackBuilder[?, ?]]
      '{
        Callbacks.handlers($path) = ${ cb }
        Callbacks.paths($cb) = ${ path }
      }
    },
    '{ () }
  )
