try_eval_tidy <- function(expr, env = rlang::caller_env()) {
  tryCatch(
    rlang::eval_tidy(expr, env = env),
    error = identity
  )
}

#' Parse a check quosure
#'
#' @param chk Quosure of a function, lambda expression, or formula.
#' @param msg Default error message (string).
#' @param syms List of arguments (symbols).
#' @param env Fallback environment, in case `chk` has empty environment.
#' @return Data frame of parsed check of check items.
#'
#' @noRd
parse_check <- function(chk, msg, syms, env) {
  chk_ev <- try_eval_tidy(chk)
  env <- capture_env(chk, env)
  if (rlang::is_formula(chk_ev)) {
    pred <- as_predicate(rlang::f_lhs(chk_ev), env)
    chk_items <- enquo_check_items(rlang::f_rhs(chk_ev), env)
  } else {
    pred <- as_predicate(chk, env)
    chk_items <- lapply(syms, rlang::new_quosure, env = env)
  }
  get_attr <- function(x) (chk_ev %@% x) %||% (pred$fn %@% x)
  expr <- get_attr("vld_pred_expr") %||% pred$expr
  if (!nzchar(msg))
    msg <- get_attr("vld_err_msg") %||% ""
  interp_msg <- get_attr("vld_interp_msg") %||% TRUE
  text <- deparse_check(expr, chk_items, msg, interp_msg, env)
  validation_tbl(pred$fn, chk_items, text)
}
# cf. [`quickdf()`](http://adv-r.had.co.nz/Profiling.html#be-lazy)
validation_tbl <- function(pred, chk_items, text) {
  n <- length(chk_items)
  x <- list(
    pred       = `[<-`(vector("list", n), list(pred)),
    expr       = chk_items,
    call       = text$call,
    msg        = text$msg,
    is_msg_gbl = text$is_msg_gbl,
    env        = text$env
  )
  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(n)
  x
}

enquo_check_items <- function(x, env) {
  if (is_quos_expr(x))
    eval(x, env)
  else if (rlang::is_quosures(x))
    x
  else
    list(rlang::new_quosure(x, env))
}
is_quos_expr <- function(x) {
  is.call(x) && identical(x[[1]], as.name("quos"))
}

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
is_lambda <- function(x) {
  is.call(x) && identical(x[[1]], as.symbol("{"))
}
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
is_error <- function(x) {
  inherits(x, "error")
}

deparse_check <- function(expr, chk_items, def_msg, interp_msg, env) {
  calls <- vapply(chk_items, deparse_call, character(1), x = expr)
  msgs <- names_filled(chk_items)
  is_gbl <- !nzchar(msgs)
  msgs[is_gbl] <-
    make_message(def_msg, interp_msg, env, chk_items[is_gbl], calls[is_gbl])
  envs <- vector("list", length(chk_items))
  envs[ is_gbl] <- list(env)
  envs[!is_gbl] <- lapply(chk_items[!is_gbl], rlang::f_env)
  list(call = calls, msg = msgs, is_msg_gbl = is_gbl, env = envs)
}
deparse_call <- function(x, arg) {
  call <- rlang::expr(UQE(x)(UQE(arg)))
  deparse_collapse(call)
}
make_message <- function(msg, interp_msg, env, chk_items, calls) {
  if (nzchar(msg)) {
    msgs <- vapply(chk_items, glue_opp, character(1), text = msg, env = env)
    if (!interp_msg)
      msgs <- protect_braces(msgs)
    msgs
  } else
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
  glue_text(text, env, list(. = rlang::quo_text(qdot)),
            .open = "{{", .close = "}}")
}
