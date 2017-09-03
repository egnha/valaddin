#' Parse validation checks
#'
#' @param ... Validation checks.
#'
#' @return List of parsed validation checks, separated by scope: global vs
#'   local. Complete parsing of global checks is deferred until the formal
#'   arguments of the function are known.
#'
#' @noRd
parse_checks <- function(...) {
  chks <- vld_spec(...)
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
  msg <- eval_tidy(msg_default)
  calls <- vapply(chk_items, function(.) deparse_call(expr, .$chk), character(1))
  msgs <- vapply(chk_items, function(.) f_rhs(.$msg), character(1))
  is_gbl <- !nzchar(msgs)
  msgs[is_gbl] <- interp_msgs(msg, env_msg, chk_items[is_gbl], calls[is_gbl])
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
  deparse_str(call)
}
interp_msgs <- function(msg, env_msg, chk_items, calls) {
  if (nzchar(msg))
    vapply(chk_items, interp_with_dot, character(1), text = msg, env = env_msg)
  else
    protect_braces_from_glue(message_false(calls))
}
interp_with_dot <- function(item, text, env) {
  dot <- quo_text(item$chk)
  interp <- glue_text(text, env, list(. = dot), .open = "{{", .close = "}}")
  len <- length(interp)
  if (len != 1) {
    text <- encodeString(text, quote = "'")
    abort(sprintf("Failed to interpolate as string: %s (length %d)", text, len))
  }
  interp
}
protect_braces_from_glue <- function(x) {
  gsub("\\}", "\\}\\}", gsub("\\{", "\\{\\{", x))
}
message_false <- function(call) {
  paste("FALSE:", call)
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
