#' Generate input validation checks
#'
#' Given a predicate (function), `localize()` returns a function that
#' _generates_ input validation checks (of local scope) for the predicate.
#' `globalize()` takes such a check generator and returns the underlying
#' predicate (wrapped in [vld()]); when `globalize()` is unquoted in a call to
#' [firmly()] or [fasten()], the corresponding predicate is applied as an input
#' validation check of global scope.
#' \cr\cr
#' The function `predicate_function()`, resp. `predicate_message()`, extracts
#' the associated predicate function, resp. error message.
#'
#' @param p Predicate function of one argument, for `localize()`, of two or more
#'   arguments for `localize_comparison()`.
#' @param msg,value Error message (string). An empty string (the default)
#'   implies that error messages are to be automatically generated.
#' @param x Object of class `local_predicate` or `global_predicate`.
#'
#' @details Conceptually, `localize()` and `globalize()` represent inverse
#'   operations. However, they are _not_ mutually invertible as functions,
#'   because they are not composable.
#'
#' @examples
#' \dontrun{
#'
#' ## Make a positivity checker
#' chk_positive <- localize({isTRUE(. > 0)}, "{{.}} is not positive")
#' f <- firmly(function(x, y) "Pass", chk_positive(x, x - y))
#'
#' f(2, 1)
#' #> [1] "Pass"
#'
#' f(1, 2)
#' #> Error: f(x = 1, y = 2)
#' #> x - y is not positive
#'
#' ## Make a length checker, parameterized by .ref
#' chk_length <-
#'   localize_comparison(
#'     {length(.) == .ref}, "{{.}} not of length {{{.ref$value}}}"
#'   )
#' ## .ref gets the value 2 (so check that the length is 2)
#' g <- firmly(function(x, y) "Pass", chk_length(2)(y))
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
localize <- fasten(
  "'msg' must be a string" := {is.character(.) && length(.) == 1} ~ msg
)(
  function(p, msg = "") {
    q <- rlang::enquo(p)
    x <- rlang::quo_expr(q)
    pred <- as_predicate(q, capture_env(q, parent.frame()))
    expr <- if (is_lambda(x)) pred$expr else x
    localize_(pred$fn, expr, msg)
  }
)
localize_ <- function(fn, expr, msg) {
  force(fn)
  force(expr)
  force(msg)
  structure(
    function(...) {
      fml <- rlang::new_formula(fn, vld(...), parent.frame())
      structure(
        (if (nzchar(msg)) vld(msg := fml) else vld(fml))[[1]],
        vld_pred_expr = expr,
        class = "local_validation_checks"
      )
    },
    class = c("local_predicate", "function")
  )
}

is_local_predicate <- function(x) {
  inherits(x, "local_predicate")
}

#' @rdname scope
#' @export
localize_comparison <- fasten(
  "'msg' must be a string" := {is.character(.) && length(.) == 1} ~ msg
)(
  function(p, msg = "") {
    force(msg)
    q <- rlang::enquo(p)
    env <- capture_env(q, parent.frame())
    cmp <- as_comparison(q, env)
    function(.ref, ...) {
      repr <- list(value = .ref, expr = deparse_collapse(substitute(.ref)))
      msg <- glue_text(msg, env, list(.ref = repr),
                       .open = "{{{", .close = "}}}")
      localize_(cmp$fn(.ref, ...), cmp$expr, msg)
    }
  }
)
as_comparison <- function(p, env) {
  expr <- rlang::f_rhs(p)
  if (is_lambda(expr)) {
    expr <- new_fn_expr(expr, alist(. = , .ref = ref))
  } else {
    f <- try_eval_tidy(p, env)
    if (!is.function(f)) {
      stop(err_not_function(expr, maybe_error(f)), call. = FALSE)
    }
    expr <- new_fn_expr(bquote(.(f)(., ref, ...)))
    env <- environment(f)
  }
  fn <- eval(new_fn_expr(expr, alist(ref = , ... = )), env)
  list(expr = expr, fn = fn)
}

#' @param chkr Function of class `local_predicate`, i.e., a function created by
#'   `localize()`.
#' @rdname scope
#' @export
globalize <- fasten(
  "'chkr' must be a local predicate (see ?localize)" :=
    is_local_predicate ~ chkr
)(
  function(chkr) {
    env <- environment(chkr)
    structure(
      (if (nzchar(env$msg)) vld(env$msg := env$fn) else vld(env$fn))[[1]],
      vld_pred_expr = env$expr,
      class = c("global_predicate", "function")
    )
  }
)

#' @rdname scope
#' @export
predicate_function <- function(x) {
  UseMethod("predicate_function")
}
#' @export
predicate_function.local_predicate <- function(x) {
  environment(x)$fn
}
#' @export
predicate_function.global_predicate <- function(x) {
  rlang::eval_tidy(x$chk)
}

#' @rdname scope
#' @export
predicate_message <- function(x) {
  UseMethod("predicate_message")
}
#' @export
predicate_message.local_predicate <- function(x) {
  environment(x)$msg
}
#' @export
predicate_message.global_predicate <- function(x) {
  rlang::eval_tidy(x$msg)
}

#' @rdname scope
#' @export
`predicate_message<-` <- function(x, value) {
  UseMethod("predicate_message<-")
}
#' @export
`predicate_message<-.local_predicate` <- function(x, value) {
  environment(x)$msg <- value
  invisible(x)
}
#' @export
`predicate_message<-.global_predicate` <- function(x, value) {
  x$msg <- rlang::new_quosure(value, parent.frame())
  invisible(x)
}

#' @export
print.local_predicate <- function(x, ...) {
  cat("<local_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(environment(x)$expr), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(predicate_message(x), quote = "\""), "\n")
  invisible(x)
}

#' @export
print.global_predicate <- function(x, ...) {
  cat("<global_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(x %@% "vld_pred_expr"), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(predicate_message(x), quote = "\""), "\n")
  invisible(x)
}
