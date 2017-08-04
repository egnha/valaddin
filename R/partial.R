#' Bare-bones partial function application
#'
#' @param `__f` Function to partially apply.
#' @param ... Argument values of `__f` to set. Quasiquotation and splicing
#'   semantics is supported.
#' @param `__first` Should the set values be placed ahead of the other
#'   arguments?
#'
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
partial <- function(`__f`, ..., `__first` = TRUE) {
  defvals <- rlang::dots_list(...)
  if (length(defvals) == 0)
    return(`__f`)
  f <- rlang::as_closure(`__f`)
  if (`__first`)
    `__subst_defvals` <- function(call)
      as.call(c(f, defvals, rlang::node_cdr(call)))
  else
    `__subst_defvals` <- function(call)
      as.call(c(f, rlang::node_cdr(call), defvals))
  `class<-`(
    function(...) {
      call <- `__subst_defvals`(sys.call())
      rlang::eval_bare(call, parent.frame())
    },
    "partial_function"
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
