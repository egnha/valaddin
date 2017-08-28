#' Generate input validation checks
#'
#' @param pred Quosure of a predicate function.
#' @param msg Quosure of an error message.
#'
#' @return Function that generates input validation checks.
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
#' ## unquote if you want to show the value of `l` instead
#' baz <- firmly(f, chk_len(l = !! len))
#' \dontrun{
#' baz(1, 2:3)}
#'
#' ## Predicate arguments can be (pre-)transformed
#' chk_with <- checker(.(., f ~ f(.)), f = rlang::as_function)
#' foobar <- firmly(f, "{{.}} is not positive" := chk_with(~ . > 0))
#' foobar(1, 2)
#' \dontrun{
#' foobar(1, 0)}
#'
#' @noRd
checker <- function(pred, msg = empty_msg) {
  pred <- as_predicate(pred)
  msg <- prioritize_err_msg(msg, pred$fn)
  text <- get_text(msg)
  env <- f_env(msg)
  `__bind_to_msg` <- function(args) {
    env_msg <- bind_expr_value(args, parent.frame(), env)
    new_quosure(text, env_msg)
  }
  `__get_env` <- f_env
  `__pred_partial` <- function(args) partial(pred$fn, args)
  `__get_exprs` <- vld_exprs
  `__pred_expr` <- function(args) as.call(c(pred$expr, args))
  fmls <- rearrange_formals(pred$fn)
  if (is_empty(names_nondot(fmls)))
    `__eval_nondot_args` <- list
  else
    `__eval_nondot_args` <- eval_nondot_args
  chkr <- function(...) {
    args <- `__eval_nondot_args`()
    msg <- `__bind_to_msg`(args)
    list(
      msg       = msg,
      env_msg   = `__get_env`(msg),
      fn        = `__pred_partial`(args),
      chk_items = `__get_exprs`(...),
      expr      = `__pred_expr`(args)
    )
  }
  formals(chkr) <- fmls
  chkr
}

as_predicate <- function(q) {
  expr <- f_rhs(q)
  if (is_lambda(expr))
    as_lambda_fn(expr, f_env(q))
  else
    list(fn = match_fn(q), expr = expr)
}
is_lambda <- function(x) {
  is.call(x) && is_block(x)
}
as_lambda_fn <- function(x, env) {
  fn <- eval_bare(new_fn_expr(x), env)
  list(fn = fn, expr = substitute(fn))
}
new_fn_expr <- function(body, args = alist(. = )) {
  call("function", as.pairlist(args), body)
}

match_fn <- function(q) {
  fn <- try_eval_tidy(q)
  if (!is.function(fn))
    abort(err_not_fn(q, fault = fn))
  fn
}
err_not_fn <- function(q, fault) {
  x <- expr_label(f_rhs(q))
  if (is_error(fault)) {
    msg <- conditionMessage(fault)
    sprintf("Error determining whether %s is a function: %s", x, msg)
  } else
    paste("Not a function:", x)
}

prioritize_err_msg <- function(first, second) {
  if (is_empty_msg(first))
    vld_err_msg(second)
  else
    first
}

#' Get or set a validation error message
#'
#' @param f Predicate function.
#'
#' @return Quosure of a string.
#'
#' @examples
#' is_integer <- is.integer
#' vld_err_msg(is_integer) <- "{{.}} not of integer type (type: {typeof(.)})"
#' vld_err_msg(is_integer)
#'
#' foo <- firmly(identity, is_integer)
#' foo(1:3)
#' \dontrun{
#' foo(runif(1:3))}
#'
#' is_integer <- rlang::is_integer
#' msg <- local({
#'   len <- function(n) if (is.null(n)) "" else paste(" of length", n)
#'   new_err_msg("{{.}} is not an integer vector{{len(.value$n)}}")
#' })
#' vld_err_msg(is_integer) <- msg
#'
#' foo <- firmly(identity, is_integer(n = 3))
#' foo(1:3)
#' \dontrun{
#' foo(1:2)}
#'
#' @export
vld_err_msg <- function(f) {
  attr(f, "valaddin_error_message_quo", exact = TRUE) %||% empty_msg
}

#' @param env Environment that is in scope when the `\{\{`-enclosed substrings
#'   of the error message are interpolated.
#' @param value Error message (string or [quosure][rlang::quosure] of a
#'   string).
#'
#' @export
#' @rdname vld_err_msg
`vld_err_msg<-` <- function(f, env = parent.frame(), value) {
  if (is_string(value))
    msg <- new_quosure(value, env)
  else if (is_quosure(value) && is_string(f_rhs(value)))
    msg <- value
  else
    abort("Error message must be a string or quosure (of a string)")
  `attr<-`(f, "valaddin_error_message_quo", msg)
}

#' @param msg Error message (string).
#'
#' @export
#' @rdname vld_err_msg
new_err_msg <- function(msg, env = parent.frame()) {
  if (!is_string(msg))
    abort("Error message much be a string")
  new_quosure(msg, env)
}

get_text <- function(q) {
  text <- f_rhs(q)
  if (!is_string(text))
    abort(paste("Not a string:", deparse_collapse(text)))
  text
}

bind_expr_value <- function(args, env, parent) {
  env_bind <- new.env(parent = parent)
  env_bind$.expr <- bind_expr_text(nomen(args), env)
  env_bind$.value <- lapply(args, deparse_collapse)
  env_bind
}
bind_expr_text <- function(syms, env) {
  lapply(syms, function(sym)
    deparse_collapse(
      eval_bare(substitute(substitute(., env), list(. = sym)))
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

rearrange_formals <- function(f) {
  fmls <- segregate_args(formals(f)[-1])
  as.pairlist(c(fmls$wo_value, alist(... = ), fmls$with_value))
}
segregate_args <- function(fmls) {
  fmls <- fmls[names(fmls) != "..."]
  wo_value <- vapply(fmls, identical, logical(1), y = quote(expr = ))
  list(wo_value = fmls[wo_value], with_value = fmls[!wo_value])
}

eval_nondot_args <- function() {
  mc <- match.call(sys.function(-1), call = sys.call(-1), expand.dots = FALSE)
  args <- nomen(mc[-1])
  lapply(args, eval_bare, env = parent.frame())
}
