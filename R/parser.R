#' Parse input validation checks
#'
#' @param chks List of input-validation checks, as output by [vld()].
#'
#' @return List of parsed input validation checks, separated by scope: global vs
#'   local. Complete parsing of global checks is deferred until the formal
#'   arguments of the function are known.
#'
#' @noRd
parse_checks <- function(chks) {
  if (is_empty(chks))
    return(NULL)
  chklist <- lapply(chks, parse_check)
  is_global <- vapply(chklist, function(.) is_empty(.$chk_items), logical(1))
  list(
    global = chklist[is_global],
    local  = tabulate_checks(chklist[!is_global])
  )
}
parse_check <- function(x) {
  c(decompose_check(x), decompose_message(x$msg))
}
decompose_message <- function(msg) {
  list(
    msg = eval_tidy(msg),
    env_msg = f_env(msg)
  )
}
decompose_check <- function(x) {
  if (is_chkr_validation(x))
    return(x[c("fn", "expr", "chk_items")])
  chk <- x$chk
  chk_eval <- try_eval_tidy(chk)
  if (is_formula(chk_eval)) {
    env <- f_env(chk_eval)
    fml <- get_check_formula(chk, chk_eval)
    c(
      as_predicate(f_lhs(fml), env),
      chk_items = list(as_check_items(f_rhs(fml), env))
    )
  } else
    c(
      as_predicate(chk, f_env(chk)),
      chk_items = list(NULL)
    )
}
get_check_formula <- function(chk, chk_eval) {
  x <- f_rhs(chk)
  if (is_formula(x))
    x
  else
    chk_eval
}
as_check_items <- function(x, env) {
  if (is_vld_expr(x))
    eval(x, env)
  else if (is_vld(x))
    x
  else
    list(set_empty_msg(new_quosure(x, env)))
}
is_vld_expr <- check_is_caller("vld")
as_predicate <- function(q, env) {
  expr <- get_expr(q)
  if (is_lambda(expr))
    as_lambda(expr, env)
  else {
    fn <- try_eval_tidy(q, env)
    if (!is.function(fn))
      abort(err_not_function(expr, maybe_error(fn)))
    list(expr = expr, fn = fn)
  }
}
is_lambda <- function(x) {
  is.call(x) && (x[[1]] == sym_brace || x[[1]] == sym_dot)
}
sym_brace <- as.symbol("{")
sym_dot   <- as.symbol(".")
as_lambda <- function(x, env) {
  if (x[[1]] == sym_brace)
    x <- new_fn_expr(x)
  else
    x[[1]] <- nofrills::fn
  fn <- eval(x, env)
  list(expr = substitute(fn), fn = fn)
}
new_fn_expr <- function(body, args = alist(. = )) {
  call("function", as.pairlist(args), body)
}
err_not_function <- function(x, fault = NULL) {
  x <- expr_label(x)
  if (is.null(fault))
    paste("Not a function:", x)
  else
    sprintf("Could not determine whether %s is a function: %s", x, fault)
}
maybe_error <- function(x) {
  if (is_error(x))
    conditionMessage(x)
  else
    NULL
}
is_error <- check_is_class("error")

check_at_args <- function(args) {
  quo_args <- lapply(args, function(.)
    set_empty_msg(new_quosure(., emptyenv()))
  )
  function(xs) {
    for (i in seq_along(xs))
      xs[[i]]$chk_items <- quo_args
    tabulate_checks(xs)
  }
}

tabulate_checks <- function(xs) {
  checks <- lapply(xs, tabulate_check)
  do.call("rbind", checks)
}
tabulate_check <- function(x) {
  text <- deparse_check(x$expr, x$chk_items, x$msg, x$env_msg)
  items <- lapply(x$chk_items, `[[`, "chk")
  as_check_tbl(x$fn, items, text)
}

deparse_check <- function(expr, chk_items, msg_default, env_msg) {
  calls <- vapply(chk_items, function(.) deparse_call(expr, .$chk), character(1))
  msgs <- vapply(chk_items, function(.) eval_tidy(.$msg), character(1))
  is_gbl <- !nzchar(msgs)
  msgs[is_gbl] <-
    make_message(msg_default, env_msg, chk_items[is_gbl], calls[is_gbl])
  envs <- vector("list", length(chk_items))
  envs[ is_gbl] <- list(env_msg)
  envs[!is_gbl] <- lapply(chk_items[!is_gbl], function(.) f_env(.$msg))
  list(
    call       = calls,
    msg        = msgs,
    is_msg_gbl = is_gbl,
    env_msg    = envs
  )
}
deparse_call <- function(expr, arg) {
  expr_arg <- quo_expr(arg)
  if (is_chkr_predicate_expr(expr))
    call <- as.call(c(node_car(expr), expr_arg, node_cdr(expr)))
  else
    call <- as.call(c(expr, expr_arg))
  deparse_collapse(call)
}
make_message <- function(msg, env_msg, chk_items, calls) {
  if (nzchar(msg))
    vapply(chk_items, function(.) glue_opp(.$chk, msg, env_msg), character(1))
  else
    protect_braces_from_glue(message_false(calls))
}
protect_braces_from_glue <- function(x) {
  gsub("\\}", "\\}\\}", gsub("\\{", "\\{\\{", x))
}
message_false <- function(call) {
  paste("FALSE:", call)
}
# glue strings, "oppositely," e.g., with `qdot` as `quo(x)`,
#   "length of {{sQuote(.)}}: {length(.)}"
# is turned into
#   "length of ‘x’: {length(.)}"
glue_opp <- function(qdot, text, env) {
  glue_text(
    text,
    env,
    list(. = quo_text(qdot)),
    .open = "{{",
    .close = "}}"
  )
}

as_check_tbl <- function(pred, items, text) {
  n <- length(items)
  x <- list(
    pred       = `[<-`(vector("list", n), list(pred)),
    expr       = items,
    call       = text$call,
    msg        = text$msg,
    env_msg    = text$env_msg,
    is_msg_gbl = text$is_msg_gbl
  )
  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(n)
  x
}
