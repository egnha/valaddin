#' Bare-bones partial function application
#'
#' @param `__f` Function to partially apply.
#' @param ... Argument values of `__f` to set. Supports splicing via `!!!`.
#' @return Function of `...` that partially applies `__f`.
#'
#' @examples
#' # version of ls() that shows all bindings in an environment
#' ls_all <- partial(ls, all.names = TRUE)
#'
#' # alternatively, supply default values in a list
#' args <- list(all.names = TRUE)
#' ls_all <- partial(ls, !!! args)
#'
#' @noRd
partial <- function(`__f`, ...) {
  force(`__f`)
  defvals <- rlang::dots_list(...)
  if (length(defvals) == 0)
    return(`__f`)
  `__subst_defvals` <- function(call)
    as.call(c(`__f`, defvals, rlang::node_cdr(call)))
  structure(
    function(...) {
      call <- `__subst_defvals`(sys.call())
      eval(call, parent.frame())
    },
    class = "partial_function"
  )
}

print.partial_function <- function(x, ...) {
  cat("<partial_function>\n")
  cat("\n* Default values:\n")
  cat(itemize_vals(environment(x)$defvals), "\n", sep = "")
  cat("\n* Original function:\n")
  print(environment(x)$`__f`)
  invisible(x)
}
itemize_vals <- function(vals) {
  vals <- lapply(vals, deparse_collapse)
  paste(sprintf("%s = %s", names(vals), vals), collapse = "\n")
}
