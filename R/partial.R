#' Bare-bones partial function application
#'
#' @param f Function to partially apply.
#' @param arg_fill Named list of argument values to fill.
#'
#' @return Function of `...` that applies `f` with the specified arguments
#'   pre-filled.
#'
#' @examples
#' # version of ls() that shows all bindings in an environment
#' ls_all <- partial(ls, list(all.names = TRUE))
#'
#' @noRd
partial <- function(f, arg_fill) {
  if (length(arg_fill) == 0)
    return(f)
  f <- rlang::as_closure(f)
  fill_args <- function() {
    arg_call <- rlang::node_cdr(sys.call(-1))
    as.call(c(f, arg_fill, arg_call))
  }
  function(...) {
    call <- fill_args()
    rlang::eval_bare(call, parent.frame())
  }
}
