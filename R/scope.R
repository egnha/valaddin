#' Generate input validation checks
#'
#' Given a predicate (function), `localize()` returns a function that
#' _generates_ input validation checks (of local scope) for the predicate.
#' `globalize()` takes such a check generator and returns the underlying error
#' message-predicate pair. When `globalize()` is unquoted in a call to
#' [firmly()] or [fasten()], the corresponding predicate is applied as an input
#' validation check of global scope.
#' \cr\cr
#' The functions `pred_function()` and `pred_message()` extract the associated
#' predicate function and error message.
#'
#' @details `globalize()` approximately inverts `localize()`, by returning the
#'   underlying message-predicate pair as a named list, rather than as a
#'   bare predicate function or definition (formula).
#'
#' @examples
#' \dontrun{
#'
#' ## Make a positivity checker
#' chk_positive <- localize("{{.}} is not positive" := {isTRUE(. > 0)})
#' f <- firmly(function(x, y) "Pass", chk_positive(x, x - y))
#'
#' f(2, 1)
#' #> [1] "Pass"
#'
#' f(1, 2)
#' #> Error: f(x = 1, y = 2)
#' #> x - y is not positive
#'
#' ## Make a parameterized length checker
#' chk_length <-
#'   localize(
#'     "{{.}} not of length {{l}}" := function(., l) length(.) == l
#'   )
#' ## l gets the value 2 (so check that the length is 2)
#' g <- firmly(function(x, y) "Pass", chk_length(l = 2, y))
#'
#' g(1, 1:2)
#' #> [1] "Pass"
#'
#' g(1:2, 1)
#' #> Error: g(x = 1)
#' #> y not of length 2
#' }
#'
#' @name scope
NULL

#' @rdname scope
#' @export
#' @param p Predicate function, or a definition, whose LHS is an error message
#'   (string) and whose RHS is a predicate function.
localize <- function(p) {
  q <- rlang::enquo(p)
  check <- as_check(q)
  pred <- as_predicate(check$chk, rlang::f_env(check$chk))
  structure(
    function(...) {
      args <- separate_defvals_exprs(...)
      fn <- partial(pred$fn, UQS(args$defvals))
      # TODO: interpolate the message
      check$chk <- rlang::new_formula(fn, vld(UQS(args$exprs)), parent.frame())
      structure(
        check,
        vld_pred_expr = as.call(c(pred$expr, args$defvals)),
        class = "local_validation_checks"
      )
    },
    class = "local_predicate"
  )
}
separate_defvals_exprs <- function(...) {
  dd <- rlang::dots_definitions(...)
  is_named <- nzchar(names(dd$dots))
  list(
    defvals = lapply(dd$dots[is_named], rlang::eval_tidy),
    exprs   = vld_(dd$dots[!is_named], dd$defs)
  )
}
as_check <- function(q) {
  if (rlang::is_definition(rlang::f_rhs(q))) {
    def <- rlang::f_rhs(q)
    env <- rlang::f_env(q)
    list(
      msg = rlang::new_quosure(rlang::f_lhs(def), env),
      chk = rlang::new_quosure(rlang::f_rhs(def), env)
    )
  } else
    set_empty_msg(q)
}

is_local_predicate <- identify_class("local_predicate")

#' @rdname scope
#' @export
#' @param lp Function of class `local_predicate`, i.e., a function created by
#'   `localize()`.
globalize <- fasten(
  "'lp' must be a local predicate (see ?localize)" :=
    is_local_predicate ~ lp
)(
  function(lp) {
    pred <- environment(lp)$pred
    check <- environment(lp)$check
    check$chk <- rlang::new_quosure(pred$fn, rlang::f_env(check$chk))
    structure(
      check,
      vld_pred_expr = pred$expr,
      class = "global_predicate"
    )
  }
)

#' @rdname scope
#' @export
#' @param x Object of class `local_predicate` or `global_predicate`.
pred_function <- function(x) {
  UseMethod("pred_function")
}
#' @export
pred_function.local_predicate <- function(x) {
  environment(x)$pred$fn
}
#' @export
pred_function.global_predicate <- function(x) {
  rlang::eval_tidy(x$chk)
}

#' @rdname scope
#' @export
pred_message <- function(x) {
  UseMethod("pred_message")
}
#' @export
pred_message.local_predicate <- function(x) {
  rlang::eval_tidy(environment(x)$check$msg)
}
#' @export
pred_message.global_predicate <- function(x) {
  rlang::eval_tidy(x$msg)
}

#' @rdname scope
#' @export
#' @param value Error message (string).
`pred_message<-` <- function(x, value) {
  UseMethod("pred_message<-")
}
#' @export
`pred_message<-.local_predicate` <- function(x, value) {
  rlang::f_rhs(environment(x)$check$msg) <- value
  invisible(x)
}
#' @export
`pred_message<-.global_predicate` <- function(x, value) {
  x$msg <- rlang::new_quosure(value, parent.frame())
  invisible(x)
}

#' @export
print.local_predicate <- function(x, ...) {
  cat("<local_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(environment(x)$pred$expr), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(pred_message(x), quote = "\""), "\n")
  invisible(x)
}

#' @export
print.global_predicate <- function(x, ...) {
  cat("<global_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(x %@% "vld_pred_expr"), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(pred_message(x), quote = "\""), "\n")
  invisible(x)
}
