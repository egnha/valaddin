#' Partial function application
#'
#' @param `__f` Function to partially apply.
#' @param ... Argument values of `__f` to set. Supports splicing via `!!!`.
#' @return Function of `...` that partially applies `__f`. (However, if no
#'   argument values are set, `__f` is simply returned.)
#'
#' @details `partial()` resolves an error in [purrr::partial()], see \pkg{purrr}
#'   issue [349](https://github.com/tidyverse/purrr/issues/349). The `.env`,
#'   `.lazy`, `.first` arguments are dropped.
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
  defvals <- rlang::dots_list(...)
  if (length(defvals) == 0)
    return(`__f`)
  f <- substitute(`__f`)
  `__subst_defvals` <- function(call)
    as.call(c(f, defvals, rlang::node_cdr(call)))
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
  cat(itemize(environment(x)$defvals), "\n", sep = "")
  cat("\n* Original function:\n")
  print(environment(x)$`__f`)
  invisible(x)
}
itemize <- function(xs) {
  xs <- lapply(xs, deparse_collapse)
  paste(paste(names(xs), xs, sep = ": "), collapse = "\n")
}
