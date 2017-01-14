#' Assign lazy expressions to an environment, as promises
#'
#' \code{lazy_assign()} takes the expressions of a list of "lazy
#' objects"—expression-environment pairs—and binds them to a common environment
#' as promises; the environment of the lazy object is used as the (evaluation)
#' environment of the promise. Typically, \code{lazy_assign()} is used to
#' gather function arguments into a special environment.
#'
#' @param .ll List of lazy objects, cf. \code{\link[lazyeval]{lazy_dots}()},
#'   \code{\link[lazyeval]{lazy}()}.
#' @param .env Environment.
#' @details The expression of each object should be a symbol, i.e., of type
#'   \code{"symbol"} not \code{"language"}.
#' @return The environment \code{env}, invisibly.
#' @keywords internal
#' @examples
#' e0 <- new.env(parent = emptyenv())
#' e0$x <- pi
#' ls.str(e0)
#'
#' ll <- list(lazyeval::lazy_(quote(x), e0), lazyeval::lazy_(quote(x + 1), e0))
#' e <- lazy_assign(ll, new.env(parent = emptyenv()))
#'
#' # Look at the promises bound to e, without evaluating them
#' lapply(lapply(ls(e), as.symbol), function(symb)
#'   eval(substitute(substitute(., e), list(. = symb)))
#' )
#'
#' # But notice that `x + 1` was indeed bound as a promise, not a value
#' ls.str(e)  # Error: could not find function "+"
lazy_assign <- function(.ll, .env) {
  for (x in .ll) {
    eval(substitute(
      delayedAssign(deparse(x$expr), ..expr.., x$env, .env),
      list(..expr.. = x$expr)
    ))
  }

  invisible(.env)
}
