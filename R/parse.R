try_eval_tidy <- function(expr, env = rlang::caller_env(), error = identity) {
  tryCatch(rlang::eval_tidy(expr, env = env), error = error)
}

parse_check <- function(chk, msg, syms) {
  env <- rlang::get_env(chk)
  chk_eval <- try_eval_tidy(chk, env = env)
  if (rlang::is_formula(chk_eval)) {
    qs <- enquo_check_items(rlang::f_rhs(chk_eval), env)
    pred <- get_predicate(rlang::f_lhs(chk_eval), env)
  } else {
    qs <- lapply(syms, rlang::new_quosure, env = env)
    pred <- get_predicate(chk, env)
  }
  if (!nzchar(msg)) {
    msg <- attr(chk_eval, "def_err_msg", exact = TRUE) %||% ""
  }
  text <- deparse_check(pred[["expr"]], qs, msg, env)
  validation_tbl(pred[["fn"]], qs, text)
}

validation_tbl <- function(pred, exprs, text) {
  n <- length(exprs)
  x <- list(
    pred        = `[<-`(vector("list", n), list(pred)),
    expr        = exprs,
    call        = text[["call"]],
    msg         = text[["msg"]],
    dot_as_expr = text[["dot_as_expr"]]
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
  is.call(x) && identical(x[[1L]], as.name("quos"))
}

get_predicate <- function(x, env) {
  if (rlang::is_quosure(x)) {
    env <- rlang::get_env(x)
  }
  x_expr <- rlang::quo_expr(x)
  if (is_lambda(x_expr)) {
    expr <- call("function", as.pairlist(alist(. = )), x_expr)
    fn <- eval(expr, env)
  } else {
    expr <- x_expr
    fn <- try_eval_tidy(x, env = env)
    if (!is.function(fn)) {
      stop(err_not_function(x, fn), call. = FALSE)
    }
  }
  list(expr = expr, fn = fn)
}

is_lambda <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("{"))
}

err_not_function <- function(x, fault) {
  if (is_error(fault)) {
    sprintf("Error trying to determine whether %s is a function: %s",
            rlang::quo_text(x), conditionMessage(fault))
  } else {
    sprintf("Not a function: %s", rlang::quo_text(x))
  }
}

deparse_check <- function(expr, qs, def_msg, env) {
  calls <- vapply(qs, deparse_call, character(1), expr = expr)
  msgs <- names_filled(qs)
  not_named <- !nzchar(msgs)
  msgs[not_named] <- generate_message(def_msg, env,
                                      qs[not_named], calls[not_named])
  list(call = calls, msg = msgs, dot_as_expr = not_named)
}

deparse_call <- function(expr, arg) {
  call <- rlang::expr(UQE(expr)(UQE(arg)))
  deparse_collapse(call)
}

generate_message <- function(def_msg, env, qs, calls) {
  if (nzchar(def_msg)) {
    vapply(qs, glue_opp, character(1), text = def_msg, env = env)
  } else {
    # double-up braces to shield them from glue::glue()
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
#' @param q Quosure.
#' @param text Text to interpolate.
#' @return Glue object, i.e., string of class `glue`.
#' @examples
#' glue_opp("The length of {{sQuote(.)}} is {length(.)}.", q = rlang::quo(x))
#' # The length of ‘x’ is {length(.)}.
glue_opp <- function(q, text, env) {
  env_dot <- new.env(parent = env)
  env_dot[["."]] <- rlang::quo_text(q)

  # substitute string into call to avoid binding string to env,
  # which could clash with a name in an environment higher up
  eval(bquote(glue::glue(.(relevel_braces(text)), .envir = env_dot))) %||%
    # work-around bug in glue 1.0.0 (get character(0) for certain strings)
    ""
}

#' Re-level curly braces
#'
#' `relevel_braces()` converts groups of a single curly braces to groups of
#' double curly braces, and vice versa. It assumes, but does not check, that
#' braces are matched.
#'
#' @noRd
#' @param text String.
#' @return String with braces re-leveled, but otherwise unchanged.
#' @examples
#' relevel_braces(".")                         # .
#' relevel_braces("{.}")                       # {{.}}
#' relevel_braces("{{.}}")                     # {.}
#' relevel_braces("{{.}.}")                    # {{{.}.}}
#' relevel_braces("{{.}{.}}")                  # {{{.}{.}}}
#' relevel_braces(".{{.}.}.{{.}}.{.{.}}.{.}")  # .{{{.}.}}.{.}.{{.{.}}}.{{.}}
relevel_braces <- function(text) {
  text_vec <- strsplit(text, NULL)[[1L]]
  paste(relevel_braces_(text_vec), collapse = "")
}

relevel_braces_ <- function(x) {
  ht <- as.integer(cumsum(brace_val(x)))
  rle <- rle(ht != 0L)
  out <- vector("list", length(rle[["values"]]))
  pos <- 0L
  for (i in seq_along(rle[["values"]])) {
    l <- rle[["lengths"]][i]
    seq <- pos + seq_len(l)
    if (rle[["values"]][i]) {
      if (is_double_brace(ht[seq])) {
        out[[i]] <- x[seq][2L:(l - 1L)]
      } else {
        out[[i]] <- c("{", x[seq], "}")
      }
    } else {
      out[[i]] <- x[seq]
    }
    pos <- pos + l
  }
  unlist(out)
}

brace_val <- function(x) {
  val <- numeric(length(x))
  val[x == "{"] <-  1
  val[x == "}"] <- -1
  val
}

is_double_brace <- function(ht) {
  if (length(ht) <= 2L) {
    FALSE
  } else {
    sum(ht == 1L) == 2L
  }
}
