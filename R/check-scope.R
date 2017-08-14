#' Generate input validation checks
#'
#' Given a predicate (function), `lcl()` returns a function that _generates_
#' input validation checks (of local scope) for the predicate. `gbl()` takes
#' such a check generator and returns the underlying error message-predicate
#' pair. When `gbl()` is unquoted in a call to [firmly()] or [fasten()], the
#' corresponding predicate is applied as an input validation check of global
#' scope.
#' \cr\cr
#' The functions `pred_fn()` and `pred_msg()` extract the associated predicate
#' function and error message.
#'
#' @details `gbl()` approximately inverts `lcl()`, by returning the underlying
#'   message-predicate pair as a named list, rather than as a bare predicate
#'   function or definition (formula).
#'
#' @examples
#' \dontrun{
#'
#' ## Make a positivity checker
#' chk_positive <- lcl("{{.}} is not positive" := {isTRUE(. > 0)})
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
#'   lcl("{{.}} not of length {{.value$l}}" := function(., l) length(.) == l)
#'
#' ## l gets the value 2 (so check that the length is 2)
#' g <- firmly(function(x, y) "Pass", chk_length(l = 2, y))
#'
#' g(1, 1:2)
#' #> [1] "Pass"
#'
#' g(1:2, 1)
#' #> Error: g(x = 1:2, y = 1)
#' #> y not of length 2
#'
#' ## To apply a local predicate to all arguments, invoke gbl() on it
#' f <- firmly(function(x, y) "Pass", gbl(vld_double(n = 1)))
#'
#' f(pi, 1)
#' #> [1] "Pass"
#'
#' f(pi, runif(3))
#' #> Error: f(x = pi, y = runif(3))
#' #> y is not a double vector of length 1
#' }
#'
#' @name check-scope
NULL

#' @rdname check-scope
#' @export
#' @param p Predicate function, or a definition, whose LHS is an error message
#'   (string) and whose RHS is a predicate function.
lcl <- function(p) {
  check <- as_check(rlang::enquo(p))
  pred <- as_predicate(check$chk, rlang::f_env(check$chk))
  new_predicate_expr <- function(args)
    as.call(c(pred$expr, args))
  update_check <- function(args, env, ...) {
    f <- partial(pred$fn, args)
    rlang::new_formula(f, vld(...), env)
  }
  update_message <- function(args, env) {
    env_msg <- bind_expr_value(args, env, rlang::f_env(check$msg))
    rlang::new_quosure(rlang::f_rhs(check$msg), env_msg)
  }
  `class<-`(
    `formals<-`(
      function() {
        args <- eval_nondot_args()
        structure(
          list(
            msg = update_message(args, environment()),
            chk = update_check(args, parent.frame(), ...)
          ),
          vld_pred_expr = new_predicate_expr(args),
          class = "local_validation_checks"
        )
      },
      value = localize_formals(pred$fn)
    ),
    "local_predicate"
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

bind_expr_value <- function(args, env, parent) {
  env_bind <- new.env(parent = parent)
  env_bind$.expr <- bind_expr_text(nomen(args), env)
  env_bind$.value <- lapply(args, deparse_collapse)
  env_bind
}
bind_expr_text <- function(nm, env) {
  lapply(`names<-`(nm$sym, nm$nm), function(sym)
    deparse_collapse(
      eval(substitute(substitute(., env), list(. = sym)))
    )
  )
}

eval_nondot_args <- function() {
  mc <- match.call(sys.function(-1), call = sys.call(-1), expand.dots = FALSE)
  nm <- nomen(mc[-1])
  lapply(`names<-`(nm$sym, nm$nm), rlang::eval_bare, env = parent.frame())
}

localize_formals <- function(f) {
  fmls <- segregate_args(formals(f)[-1])
  as.pairlist(c(fmls$wo_value, alist(... = ), fmls$with_value))
}
segregate_args <- function(fmls) {
  fmls <- fmls[names(fmls) != "..."]
  wo_value <- vapply(fmls, identical, logical(1), y = quote(expr = ))
  list(wo_value = fmls[wo_value], with_value = fmls[!wo_value])
}

is_local_predicate <- identify_class("local_predicate")

#' @rdname check-scope
#' @export
#' @param x Function of class `local_validation_checks`.
gbl <- fasten(
  "'x' must be a local validation check" :=
    {inherits(., "local_validation_checks")} ~ x
)(
  function(x) {
    pred <- rlang::f_lhs(x$chk)
    structure(
      list(
        msg = x$msg,
        chk = rlang::new_quosure(pred, rlang::f_env(x$chk))
      ),
      vld_pred_expr = x %@% "vld_pred_expr",
      class = "global_predicate"
    )
  }
)

#' @rdname check-scope
#' @export
#' @param x Object of class `local_predicate` or `global_predicate`.
pred_fn <- function(x) {
  UseMethod("pred_fn")
}
#' @export
pred_fn.local_predicate <- function(x) {
  environment(x)$pred$fn
}
#' @export
pred_fn.global_predicate <- function(x) {
  rlang::eval_tidy(x$chk)
}

#' @rdname check-scope
#' @export
pred_msg <- function(x) {
  UseMethod("pred_msg")
}
#' @export
pred_msg.local_predicate <- function(x) {
  rlang::eval_tidy(environment(x)$check$msg)
}
#' @export
pred_msg.global_predicate <- function(x) {
  rlang::eval_tidy(x$msg)
}

#' @rdname check-scope
#' @export
#' @param value Error message (string).
`pred_msg<-` <- function(x, value) {
  UseMethod("pred_msg<-")
}
#' @export
`pred_msg<-.local_predicate` <- function(x, value) {
  environment(x)$check$msg <- rlang::new_quosure(value, parent.frame())
  invisible(x)
}
#' @export
`pred_msg<-.global_predicate` <- function(x, value) {
  x$msg <- rlang::new_quosure(value, parent.frame())
  invisible(x)
}

#' @export
print.local_predicate <- function(x, ...) {
  cat("<local_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(environment(x)$pred$expr), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(pred_msg(x), quote = "\""), "\n")
  invisible(x)
}

#' @export
print.global_predicate <- function(x, ...) {
  cat("<global_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(x %@% "vld_pred_expr"), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(pred_msg(x), quote = "\""), "\n")
  invisible(x)
}
