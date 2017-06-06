try_eval_tidy <- function(expr, data = NULL, env = rlang::caller_env()) {
  tryCatch(rlang::eval_tidy(expr, data, env), error = function(e) NULL)
}

parse_check <- function(chk, msg, syms) {
  env <- rlang::get_env(chk)
  if (rlang::is_formula(chk_eval <- try_eval_tidy(chk, env = env))) {
    chk <- quo_predicate(chk_eval, env)
    qs <- quo_check_items(chk_eval, env)
  } else {
    qs <- lapply(syms, rlang::new_quosure, env = env)
  }
  pred <- lambda(chk)
  text <- deparse_check(pred, qs, msg, env)
  validation_df(pred, qs, text)
}

quo_predicate <- function(f, env) {
  lhs <- rlang::f_lhs(f)
  if (rlang::is_quosure(lhs)) {
    lhs
  } else {
    rlang::new_quosure(rlang::quo_expr(lhs), env)
  }
}

quo_check_items <- function(f, env) {
  rhs <- rlang::f_rhs(f)
  if (is_quos(rhs)) {
    rlang::eval_tidy(rhs, env = env)
  } else if (rlang::is_quosures(rhs)) {
    rhs
  } else {
    list(rlang::new_quosure(rhs, env))
  }
}

is_quos <- function(x) {
  is.call(x) && identical(x[[1L]], as.name("quos"))
}

validation_df <- function(pred, exprs, text) {
  n <- length(exprs)
  d <- list(
    pred        = `[<-`(vector("list", n), list(pred)),
    expr        = exprs,
    call        = text[["call"]],
    msg         = text[["msg"]],
    dot_as_expr = text[["dot_as_expr"]]
  )
  class(d) <- "data.frame"
  attr(d, "row.names") <- .set_row_names(n)
  d
}

# Variation of rlang::as_function
lambda <- function(q) {
  body <- rlang::quo_expr(q)
  if (is_lambda(body)) {
    expr <- call("function", as.pairlist(alist(. = )), body)
    rlang::new_quosure(expr, rlang::get_env(q))
  } else if (is.function(try_eval_tidy(q))) {
    q
  } else {
    stop("Not a function (or quosure thereof): ", rlang::quo_text(q),
         call. = FALSE)
  }
}

deparse_check <- function(pred, qs, default, env) {
  calls <- vapply(qs, deparse_call, character(1), q = pred)
  msgs <- names(qs) %||% character(length(qs))
  not_named <- !nzchar(msgs)
  msgs[not_named] <- generate_message(default, env,
                                      qs[not_named], calls[not_named])
  list(call = calls, msg = msgs, dot_as_expr = not_named)
}

deparse_call <- function(q, arg) {
  call <- rlang::quo_expr(rlang::expr(UQE(q)(UQ(arg))))
  deparse_collapse(call)
}

generate_message <- function(default, env, qs, calls) {
  if (nzchar(default)) {
    vapply(qs, glue_opp, character(1), text = default, env = env)
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
