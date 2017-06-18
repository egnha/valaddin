try_eval_tidy <- function(expr, env = rlang::caller_env(), error = identity) {
  tryCatch(rlang::eval_tidy(expr, env = env), error = error)
}

parse_check <- function(chk, msg, syms) {
  env <- rlang::get_env(chk)
  chk_eval <- try_eval_tidy(chk, error = function(e) NULL)
  if (rlang::is_formula(chk_eval) && ! rlang::is_quosure(chk_eval)) {
    qs <- enquo_check_items(rlang::f_rhs(chk_eval), env)
    pred <- as_predicate(rlang::f_lhs(chk_eval), env)
  } else {
    qs <- lapply(syms, rlang::new_quosure, env = env)
    pred <- as_predicate(chk, env)
  }
  if (!nzchar(msg)) {
    msg <- attr(chk_eval, "def_err_msg", exact = TRUE) %||% ""
  }
  protect <- attr(chk_eval, "protect_msg", exact = TRUE) %||% FALSE
  text <- deparse_check(pred[["expr"]], qs, msg, protect, env)
  validation_tbl(pred[["fn"]], qs, text)
}

# stripped-down version of tibble:::list_to_tibble()
validation_tbl <- function(pred, exprs, text) {
  n <- length(exprs)
  x <- list(
    pred       = `[<-`(vector("list", n), list(pred)),
    expr       = exprs,
    call       = text[["call"]],
    msg        = text[["msg"]],
    is_msg_gbl = text[["is_msg_gbl"]],
    env        = text[["env"]]
  )
  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(n)
  x
}

enquo_check_items <- function(expr, env) {
  if (is_quos(expr)) {
    rlang::eval_tidy(expr, env = env)
  } else if (rlang::is_quosures(expr)) {
    expr
  } else {
    list(rlang::new_quosure(expr, env))
  }
}

is_quos <- function(x) {
  is.call(x) && identical(x[[1]], as.name("quos"))
}

as_predicate <- function(x, env) {
  if (rlang::is_quosure(x)) {
    env <- rlang::get_env(x)
  }
  expr <- rlang::get_expr(x)
  if (is_lambda(expr)) {
    expr <- function_expr(expr)
    fn <- eval(expr, env)
  } else {
    fn <- try_eval_tidy(x, env)
    if (!is.function(fn)) {
      stop(err_not_function(x, fn), call. = FALSE)
    }
  }
  list(expr = expr, fn = fn)
}

is_lambda <- function(x) {
  is.call(x) && identical(x[[1]], as.symbol("{"))
}

function_expr <- function(body, args = alist(. = )) {
  call("function", as.pairlist(args), body)
}

err_not_function <- function(x, fault) {
  if (is_error(fault)) {
    sprintf("Error trying to determine whether %s is a function: %s",
            rlang::quo_text(x), conditionMessage(fault))
  } else {
    sprintf("Not a function: %s", rlang::quo_text(x))
  }
}

is_error <- function(x) inherits(x, "error")

deparse_check <- function(expr, qs, def_msg, protect, env) {
  calls <- vapply(qs, deparse_call, character(1), expr = expr)
  msgs <- names_filled(qs)
  is_msg_gbl <- !nzchar(msgs)
  msgs[is_msg_gbl] <-
    generate_message(def_msg, protect, env, qs[is_msg_gbl], calls[is_msg_gbl])
  envs <- vector("list", length(qs))
  envs[ is_msg_gbl] <- list(env)
  envs[!is_msg_gbl] <- lapply(qs[!is_msg_gbl], rlang::get_env)
  list(call = calls, msg = msgs, is_msg_gbl = is_msg_gbl, env = envs)
}

deparse_call <- function(expr, arg) {
  call <- rlang::expr(UQE(expr)(UQE(arg)))
  deparse_collapse(call)
}

generate_message <- function(def_msg, protect, env, qs, calls) {
  if (nzchar(def_msg)) {
    msg <- vapply(qs, glue_opp, character(1), text = def_msg, env = env)
    if (protect) {
      msg <- double_braces(msg)
    }
    msg
  } else {
    # double-up braces to shield them from glue_text()
    double_braces(message_false(calls))
  }
}

double_braces <- function(x) {
  gsub("\\}", "\\}\\}", gsub("\\{", "\\{\\{", x))
}

message_false <- function(call) {
  sprintf("FALSE: %s", call)
}

#' Glue strings, oppositely
#'
#' `glue_opp()` is an opposite version of [glue::glue()] for a designated
#' expression (`.`): expressions in double curly braces are interpolated, while
#' those in single curly braces are literally interpreted.
#'
#' @noRd
#' @param q Quosure whose flattened string representation is the value of `.`.
#' @param text Text to interpolate.
#' @param env Environment in which to evaluate [glue::glue()].
#' @return Glue object, i.e., string of class `glue`.
#' @examples
#' q <- rlang::quo(x)
#' glue_opp(q, "The length of {{sQuote(.)}} is {length(.)}.", baseenv())
#' # The length of ‘x’ is {length(.)}.
glue_opp <- function(q, text, env) {
  glue_text(text, env, list(. = rlang::quo_text(q)),
            .open = "{{", .close = "}}")
}
