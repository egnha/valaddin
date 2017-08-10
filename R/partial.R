#' Bare-bones partial function application
#'
#' @param f Function to partially apply.
#' @param vals Named list of argument values of `f` to set.
#'
#' @return Function of `...` that applies `f` with `vals` pre-filled.
#'
#' @examples
#' # version of ls() that shows all bindings in an environment
#' ls_all <- partial(ls, list(all.names = TRUE))
#'
#' @noRd
partial <- function(f, vals) {
  if (length(vals) == 0)
    return(f)
  f <- rlang::as_closure(f)
  fill_args <- function(call)
    as.call(c(f, vals, rlang::node_cdr(call)))
  `class<-`(
    function(...) {
      call <- fill_args(sys.call())
      rlang::eval_bare(call, parent.frame())
    },
    "partial_function"
  )
}

#' @export
print.partial_function <- function(x, ...) {
  cat("<partial_function>\n")
  cat("\n* Pre-filled values:\n")
  cat(itemize_vals(environment(x)$vals), "\n", sep = "")
  cat("\n* Original function:\n")
  print(environment(x)$f)
  invisible(x)
}
itemize_vals <- function(vals) {
  vals <- lapply(vals, deparse_collapse)
  paste(sprintf("%s = %s", names(vals), vals), collapse = "\n")
}
