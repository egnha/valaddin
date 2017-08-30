#' Parse input validation checks
#'
#' @param ... Input-validation checks.
#'
#' @return List of parsed input validation checks, separated by scope: global vs
#'   local. Complete parsing of global checks is deferred until the formal
#'   arguments of the function are known.
#'
#' @noRd
parse_checks <- function(...) {
  chks <- vld_checks(...)
  if (is_empty(chks))
    return(NULL)
  chkrs <- lapply(chks, as_checker)
  is_gbl <- vapply(chkrs, has_no_check_items, logical(1))
  list(
    global = chkrs[is_gbl],
    local  = tabulate_checks(chkrs[!is_gbl])
  )
}
as_checker <- function(chk) {
  call <- f_rhs(chk$chk)
  env <- f_env(chk$chk)
  pred <- new_quosure(lang_head(call), env)
  call[[1]] <- checker(pred, chk$msg)
  eval(call, env)
}
has_no_check_items <- function(chkr) {
  is_empty(chkr$chk_items)
}

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
  msg_default <- eval_tidy(msg_default)
  calls <- vapply(chk_items, function(.) deparse_call(expr, .$chk), character(1))
  msgs <- vapply(chk_items, function(.) f_rhs(.$msg), character(1))
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
  call <- as.call(c(node_car(expr), expr_arg, node_cdr(expr)))
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
