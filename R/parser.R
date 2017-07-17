#' Parse input validation checks
#'
#' @param chks List of input-validation checks as pairs of quosures (named
#'   `msg`, `chk`), as output by [vld()].
#' @return List of parsed input validation checks, separated by scope (global vs
#'   local). Complete parsing of global checks is deferred until the formal
#'   arguments of the function are known.
#' @noRd
parse_checks <- function(chks) {
  if (length(chks) == 0)
    return(NULL)
  chklist <- lapply(chks, parse_check)
  is_global <- vapply(chklist, function(.) is.null(.$chk_items), logical(1))
  list(
    global = chklist[is_global],
    local  = tabulate_checks(chklist[!is_global])
  )
}
parse_check <- function(.) {
  chkev <- try_eval_tidy(.$chk)
  if (rlang::is_formula(chkev)) {
    env_fml <- rlang::f_env(chkev)
    pred <- as_predicate(rlang::f_lhs(chkev), env_fml)
    chk_items <- as_check_items(rlang::f_rhs(chkev), env_fml)
  } else {
    pred <- as_predicate(.$chk, rlang::f_env(.$chk))
    chk_items <- NULL
  }
  msg <- rlang::eval_tidy(.$msg)
  list(
    fn        = pred$fn,
    expr      = (. %@% "vld_pred_expr") %||% pred$expr,
    chk_items = chk_items,
    msg       = msg,
    env_msg   = rlang::f_env(.$msg)
  )
}
as_check_items <- function(x, env) {
  if (is_vld_expr(x))
    eval(x, env)
  else if (is_vld(x))
    x
  else
    list(set_empty_msg(rlang::new_quosure(x, env)))
}
is_vld_expr <- identify_caller("vld")
as_predicate <- function(q, env) {
  expr <- rlang::get_expr(q)
  if (is_lambda(expr)) {
    expr <- new_fn_expr(expr)
    fn <- eval(expr, env)
  } else {
    fn <- try_eval_tidy(q, env)
    if (!is.function(fn))
      stop(err_not_function(expr, maybe_error(fn)), call. = FALSE)
  }
  list(expr = expr, fn = fn)
}
is_lambda <- identify_caller("{")
new_fn_expr <- function(body, args = alist(. = )) {
  call("function", as.pairlist(args), body)
}
err_not_function <- function(x, fault = NULL) {
  x <- rlang::expr_label(x)
  if (is.null(fault))
    sprintf("Not a function: %s", x)
  else
    sprintf("Could not determine whether %s is a function: %s", x, fault)
}
maybe_error <- function(x) {
  if (is_error(x))
    conditionMessage(x)
  else
    NULL
}
is_error <- identify_class("error")

#' Tabulate an input validation check as a data frame
#'
#' @details Adapted from
#'   [`quickdf()`](http://adv-r.had.co.nz/Profiling.html#be-lazy).
#' @param pred Predicate function.
#' @param chk_quos List of expressions to validate, as quosures.
#' @param text List of textual data for input validation:
#'   - `call`: expression of a validation call (string)
#'   - `msg`: error message (string)
#'   - `is_msg_gbl`: is the error message derived from a global error message?
#'   - `env`: environment in which to interpolate the error message
#' @return Data frame of input-validation data to be used by
#'   `validation_closure()`.
#' @noRd
validation_tbl <- function(pred, chk_quos, text) {
  n <- length(chk_quos)
  x <- list(
    pred       = `[<-`(vector("list", n), list(pred)),
    expr       = chk_quos,
    call       = text$call,
    msg        = text$msg,
    env_msg    = text$env_msg,
    is_msg_gbl = text$is_msg_gbl
  )
  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(n)
  x
}
tabulate_checks <- function(xs) {
  parts <- lapply(xs, function(.) {
    text <- deparse_check(.$expr, .$chk_items, .$msg, .$env_msg)
    validation_tbl(.$fn, lapply(.$chk_items, `[[`, "chk"), text)
  })
  do.call("rbind", parts)
}
deparse_check <- function(expr, chk_items, def_msg, env_msg) {
  calls <-
    vapply(chk_items, function(.) deparse_call(expr, .$chk), character(1))
  msgs <-
    vapply(chk_items, function(.) rlang::eval_tidy(.$msg), character(1))
  is_gbl <- !nzchar(msgs)
  msgs[is_gbl] <-
    make_message(def_msg, env_msg, chk_items[is_gbl], calls[is_gbl])
  envs <- vector("list", length(chk_items))
  envs[ is_gbl] <- list(env_msg)
  envs[!is_gbl] <- lapply(chk_items[!is_gbl], function(.) rlang::f_env(.$msg))
  list(call = calls, msg = msgs, is_msg_gbl = is_gbl, env_msg = envs)
}
deparse_call <- function(x, arg) {
  call <- rlang::expr(UQE(x)(UQE(arg)))
  deparse_collapse(call)
}
make_message <- function(msg, env_msg, chk_items, calls) {
  if (nzchar(msg))
    vapply(chk_items, function(.) glue_opp(.$chk, msg, env_msg), character(1))
  else
    # double-up braces to shield them from glue_text()
    protect_braces(message_false(calls))
}
protect_braces <- function(x) {
  gsub("\\}", "\\}\\}", gsub("\\{", "\\{\\{", x))
}
message_false <- function(call) {
  sprintf("FALSE: %s", call)
}
# glue strings, "oppositely," e.g., with `qdot` as `quo(x)`,
#   "length of {{sQuote(.)}}: {length(.)}"
# is turned into
#   "length of ‘x’: {length(.)}"
glue_opp <- function(qdot, text, env) {
  glue_text(
    text, env, list(. = rlang::quo_text(qdot)),
    .open = "{{", .close = "}}"
  )
}

localize_at <- function(xs, syms) {
  qs <- lapply(syms, function(.)
    set_empty_msg(rlang::new_quosure(., emptyenv())))
  for (i in seq_along(xs))
    xs[[i]]$chk_items <- qs
  tabulate_checks(xs)
}
