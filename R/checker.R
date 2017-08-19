#' Generate input validation checks
#'
#' Given a predicate function, `checker()` makes a function that _generates_
#' input validation checks for use with [firmly()] or [fasten()]. The functions
#' `chkr_predicate()` and `chkr_message()` extract the associated predicate
#' function and error message.
#'
#' @param p Predicate function or a definition whose LHS is an error message
#'   (string) and RHS is a predicate function.
#'
#' @return Function that generates input validation checks.
#'
#' @section Specifying error messages: TODO
#'   - double \code{\{\{} vs single \code{\{}
#'   - `.expr` pronoun
#'   - `.value` pronoun
#'
#' @seealso [Boolean checkers][checker-boolean],
#'   [Object checkers][checker-object],
#'   [Pattern checkers][checker-pattern],
#'   [Property checkers][checker-property],
#'   [Relation checkers][checker-relation],
#'   [Scalar type checkers][checker-scalar-type],
#'   [Set comparison checkers][checker-sets],
#'   [Type checkers][checker-type]
#'
#' @examples
#' f <- function(x, y) "Pass"
#'
#' ## Make a positivity checker
#' chk_pos <- checker("{{.}} is not positive" := {isTRUE(. > 0)})
#' foo <- firmly(f, chk_pos(x, x - y))
#'
#' foo(2, 1)
#' \dontrun{
#' foo(1, 2)}
#'
#' ## Make a parameterized length checker
#' msg <- "{{.}} not of length {{.value$l}} (actual length: {length(.)})"
#' chk_len <- checker(msg := function(., l) length(.) == l)
#'
#' ## Equivalently, use abbreviated notation for anonymous functions
#' chk_len <- checker(msg := .(., l ~ length(.) == l))
#'
#' bar <- firmly(f, chk_len(l = 2, y))
#'
#' bar(1, 1:2)
#' \dontrun{
#' bar(1:2, 1)}
#'
#' ## Apply a checker to all function arguments, by specifying none explicitly
#' baz <- firmly(f, chk_len(1))
#'
#' baz(1, 2)
#' \dontrun{
#' baz(1, 2:3)}
#'
#' ## Rewrite the error message
#' chkr_message(chk_len) <- "Length of {{.}} is {length(.)} not {{.expr$l}}"
#'
#' len <- 1
#' baz <- firmly(f, chk_len(l = len))
#' \dontrun{
#' baz(1, 2:3)}
#'
#' ## Since the error message encodes the expression of `l` (i.e., `.expr$l`),
#' ## unquote if you want to show the value of `l`
#' baz <- firmly(f, chk_len(l = !! len))
#' \dontrun{
#' baz(1, 2:3)}
#'
#' @export
checker <- function(p) {
  `__chkr_chk` <- as_check(enquo(p))
  `__chkr_pred` <- as_predicate(`__chkr_chk`$chk, f_env(`__chkr_chk`$chk))
  `__msg` <- function(args, env) {
    env_msg <- bind_expr_value(args, env, f_env(`__chkr_chk`$msg))
    msg <- f_rhs(`__chkr_chk`$msg)
    new_quosure(msg, env_msg)
  }
  `__fn` <- function(args) {
    partial(`__chkr_pred`$fn, args)
  }
  `__chk_items` <- vld
  `__expr` <- function(args) {
    call_with_args <- as.call(c(`__chkr_pred`$expr, args))
    as_chkr_predicate_expr(call_with_args)
  }
  chkr <- function(...) {
    args <- `__eval_nondot_args`()
    `__as_chkr_validation`(
      msg       = `__msg`(args, environment()),
      fn        = `__fn`(args),
      chk_items = `__chk_items`(...),
      expr      = `__expr`(args)
    )
  }
  formals(chkr) <- rearrange_formals(`__chkr_pred`$fn)
  class(chkr) <- "valaddin_checker"
  chkr
}

as_check <- function(q) {
  if (is_definition(f_rhs(q))) {
    def <- f_rhs(q)
    env <- f_env(q)
    list(
      msg = new_quosure(f_lhs(def), env),
      chk = new_quosure(f_rhs(def), env)
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

partial <- function(f, arg_fill) {
  if (is_empty(arg_fill))
    return(f)
  f <- as_closure(f)
  fill_args <- function() {
    arg_call <- node_cdr(sys.call(-1))
    as.call(c(f, arg_fill, arg_call))
  }
  function(...) {
    call <- fill_args()
    eval_bare(call, parent.frame())
  }
}

`__eval_nondot_args` <- function() {
  mc <- match.call(sys.function(-1), call = sys.call(-1), expand.dots = FALSE)
  nm <- nomen(mc[-1])
  lapply(`names<-`(nm$sym, nm$nm), eval_bare, env = parent.frame())
}

rearrange_formals <- function(f) {
  fmls <- segregate_args(formals(f)[-1])
  as.pairlist(c(fmls$wo_value, alist(... = ), fmls$with_value))
}
segregate_args <- function(fmls) {
  fmls <- fmls[names(fmls) != "..."]
  wo_value <- vapply(fmls, identical, logical(1), y = quote(expr = ))
  list(wo_value = fmls[wo_value], with_value = fmls[!wo_value])
}

as_chkr_predicate_expr <- make_as("chkr_predicate_expr")
is_chkr_predicate_expr <- check_is("chkr_predicate_expr")

as_chkr_validation <- make_as("chkr_validation")
`__as_chkr_validation` <- function(...) {
  as_chkr_validation(list(...))
}
is_chkr_validation <- check_is("chkr_validation")

is_local_predicate <- check_is_class("valaddin_checker")

#' @export
print.valaddin_checker <- function(x, ...) {
  cat("<valaddin_checker>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(chkr_expr(x)), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(chkr_message(x), quote = "\""), "\n")
  invisible(x)
}

chkr_expr <- function(x) {
  environment(x)$`__chkr_pred`$expr
}

#' @rdname checker
#' @export
chkr_message <- function(x) {
  eval_tidy(environment(x)$`__chkr_chk`$msg)
}

#' @rdname checker
#' @param x Function created by `checker()`.
#' @export
chkr_predicate <- function(x) {
  environment(x)$`__chkr_pred`$fn
}

#' @rdname checker
#' @param env Environment of the error message `value`.
#' @param value Error message.
#' @export
`chkr_message<-` <- function(x, env = parent.frame(), value) {
  environment(x)$`__chkr_chk`$msg <- new_quosure(value, env)
  invisible(x)
}
