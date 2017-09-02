#' Generate input validation checks
#'
#' @param pred Quosure of a predicate function.
#' @param msg Quosure of an error message.
#'
#' @return Function that generates input validation checks.
#'
#' @noRd
checker <- function(pred, msg = empty_msg) {
  pred <- as_predicate(pred)
  msg <- prioritize_err_msg(msg, pred$fn)
  fmls <- rearrange_formals(pred$fn)
  nms_pred_params <- names_nondot(fmls)
  if (is_empty(nms_pred_params)) {
    `__bind_to_msg` <- function(args) msg
    `__eval_nondot_args` <- list
  } else {
    `__bind_to_msg` <- function(args) {
      fmls <- fmls[nms_pred_params]
      fmls[names(args)] <- args
      env_msg <- bind_expr_value(fmls, parent.frame(), f_env(msg))
      new_quosure(get_text(msg), env_msg)
    }
    `__eval_nondot_args` <- eval_nondot_args
  }
  `__get_env`      <- f_env
  `__get_exprs`    <- vld_exprs
  `__pred_partial` <- function(args) partial(pred$fn, args)
  `__pred_expr`    <- function(args) as.call(c(pred$expr, args))
  `formals<-`(
    function(...) {
      args <- `__eval_nondot_args`()
      msg <- `__bind_to_msg`(args)
      list(
        msg       = msg,
        env_msg   = `__get_env`(msg),
        fn        = `__pred_partial`(args),
        expr      = `__pred_expr`(args),
        chk_items = `__get_exprs`(...)
      )
    },
    value = fmls
  )
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
  fn <- eval(new_fn_expr(x), env)
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
    error_msg(second)
  else
    first
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

bind_expr_value <- function(xs, env, parent) {
  env_bind <- new.env(parent = parent)
  env_bind$.expr <- bind_expr_text(nomen(xs), env)
  env_bind$.value <- lapply(xs, deparse_str)
  env_bind
}
bind_expr_text <- function(syms, env) {
  lapply(syms, function(sym)
    deparse_str(eval(substitute(substitute(., env), list(. = sym))))
  )
}

get_text <- function(q) {
  text <- f_rhs(q)
  if (!is_string(text))
    abort(paste("Not a string:", deparse_str(text)))
  text
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
    eval(call, parent.frame())
  }
}

eval_nondot_args <- function() {
  mc <- match.call(sys.function(-1), call = sys.call(-1), expand.dots = FALSE)
  args <- nomen(mc[-1])
  lapply(args, eval, envir = parent.frame())
}

#' Get or set a validation error message
#'
#' @param f Predicate function.
#'
#' @return Quosure of a string.
#'
#' @examples
#' is_integer <- rlang::as_closure(is.integer)
#' error_msg(is_integer) <- "{{.}} not of integer type (type: {typeof(.)})"
#' error_msg(is_integer)
#'
#' foo <- firmly(identity, is_integer)
#' foo(1:3)
#' \dontrun{
#' foo(runif(3))}
#'
#' is_integer <- rlang::is_integer
#' msg <- local({
#'   len <- function(n) if (is.null(n)) "" else paste(" of length", n)
#'   new_error_msg("{{.}} is not an integer vector{{len(.value$n)}}")
#' })
#' error_msg(is_integer) <- msg
#'
#' foo <- firmly(identity, is_integer(n = 3))
#' foo(1:3)
#' \dontrun{
#' foo(1:2)}
#'
#' @export
error_msg <- function(f) {
  environment(f)$`__valaddin_error_message` %||% empty_msg
}

#' @param env Environment that is in scope when a `\{\{...\}\}` substring of the
#'   error message is interpolated.
#' @param value Error message (string or [quosure][rlang::quosure] of a
#'   string).
#'
#' @details An error message can only be set for predicate functions that are
#'   [closures][base::function]. To set an error message of a
#'   [primitive][base::Primitive] predicate, e.g., `is.array()`, transform it to
#'   a closure with [rlang::as_closure()].
#'
#' @export
#' @rdname error_msg
`error_msg<-` <- function(f, env = parent.frame(), value) {
  if (!is_closure(f))
    abort("Can only set error message for predicates that are closures")
  if (!is.environment(env))
    abort("'env' must be an environment")
  if (is_string(value))
    msg <- new_quosure(value, env)
  else if (is_quosure(value) && is_string(f_rhs(value)))
    msg <- value
  else
    abort("Error message must be a string or quosure (of a string)")
  env_msg <- new.env(parent = environment(f))
  env_msg$`__valaddin_error_message` <- msg
  environment(f) <- env_msg
  invisible(f)
}

#' @param msg Error message (string).
#'
#' @export
#' @rdname error_msg
new_error_msg <- function(msg, env = parent.frame()) {
  if (!is_string(msg))
    abort("Error message much be a string")
  if (!is.environment(env))
    abort("'env' must be an environment")
  new_quosure(msg, env)
}
