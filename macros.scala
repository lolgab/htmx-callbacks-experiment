package library
package macros

import scala.quoted.*

def initializeMacroImpl[T](using Type[T], Quotes): Expr[Unit] =
  import quotes.reflect.*

  def isStatic(symbol: Symbol): Boolean =
    symbol.flags.is(Flags.Package) ||
      symbol.flags.is(Flags.Final) && isStatic(symbol.owner)

  val symbol = TypeRepr.of[T].typeSymbol

  val callbacksAndNames =
    symbol.declarations
      .filter(d =>
        d.isTerm &&
          d.typeRef.baseClasses
            .exists(c => c.fullName == "library.CallbackBuilder")
      )
      .map(d =>
        val prettyName = d.fullName.replace('.', '/')
        (d -> Expr(prettyName))
      )

  Expr.block(
    callbacksAndNames.map { case (callback, name) =>
      val path = '{ s"/api/${${ name }}" }
      val cb = Ref(callback).asExprOf[library.CallbackBuilder[?]]
      '{
        Callbacks.handlers($path) = ${ cb }
        Callbacks.paths($cb) = ${ path }
      }
    },
    '{ () }
  )
