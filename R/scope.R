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
  update_check <- function(args, env) {
    f <- partial(pred$fn, UQS(args$defvals))
    rlang::new_formula(f, vld(UQS(args$exprs)), env)
  }
  update_message <- function(prom) {
    env <- bind_names_values(prom, rlang::f_env(check$msg))
    rlang::new_quosure(rlang::f_rhs(check$msg), env)
  }
  structure(
    `formals<-`(
      function() {
        prom <- make_promises()
        args <- separate_defvals_exprs(...)
        structure(
          list(
            msg = update_message(prom),
            chk = update_check(args, parent.frame())
          ),
          vld_pred_expr = as.call(c(pred$expr, args$defvals)),
          class = "local_validation_checks"
        )
      },
      value = localize_formals(pred$fn)
    ),
    class = "local_predicate"
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

bind_names_values <- function(env, parent) {
  env_bind <- new.env(parent = parent)
  bindings <- nomen(env)
  env_bind$.name <- bind_names(bindings, env)
  env_bind$.value <- bind_values(bindings, env)
  env_bind
}
bind_names <- function(bindings, env) {
  lapply(`names<-`(bindings$sym, bindings$nm), function(x)
    eval(substitute(substitute(.x, env), list(.x = x)))
  )
}
bind_values <- function(bindings, env) {
  expr_vals <- lapply(bindings$sym, function(x) bquote(deparse_collapse(.(x))))
  bind_promises(bindings$nm, expr_vals, env, emptyenv())
}

make_promises <- function() {
  f <- sys.function(-1)
  call <- sys.call(-1)
  eval_bare(`[[<-`(call, 1, promiser(f)), parent.frame(2))
}
promiser <- function(f) {
  fn_promiser <- call("function", formals(f), quote(environment()))
  eval(fn_promiser, environment(f))
}

separate_defvals_exprs <- function(...) {
  args <- get_nondot_args()
  dd <- rlang::dots_definitions(...)
  is_named <- nzchar(names(dd$dots))
  list(
    defvals = c(args, lapply(dd$dots[is_named], rlang::eval_tidy)),
    exprs   = vld_(dd$dots[!is_named], dd$defs)
  )
}
get_nondot_args <- function() {
  mc <- match.call(sys.function(-2), call = sys.call(-2), expand.dots = FALSE)
  mc <- rlang::node_cdr(mc)
  lapply(mc[names(mc) != "..."], eval, envir = parent.frame(2))
}

localize_formals <- function(f) {
  fmls <- segregate_args(rlang::node_cdr(formals(f)))
  as.pairlist(c(fmls$wo_value, alist(... = ), fmls$with_value))
}
segregate_args <- function(fmls) {
  fmls <- fmls[names(fmls) != "..."]
  wo_value <- vapply(fmls, identical, logical(1), y = quote(expr = ))
  list(wo_value = fmls[wo_value], with_value = fmls[!wo_value])
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
