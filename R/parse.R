try_eval_tidy <- function(expr, env = rlang::caller_env(), error = identity) {
  tryCatch(rlang::eval_tidy(expr, env = env), error = error)
}

parse_check <- function(chk, msg, syms) {
  env <- rlang::get_env(chk)
  chk_eval <- try_eval_tidy(chk, error = const(NULL))
  if (rlang::is_formula(chk_eval) && ! rlang::is_quosure(chk_eval)) {
    qs <- enquo_check_items(rlang::f_rhs(chk_eval), env)
    pred <- get_predicate(rlang::f_lhs(chk_eval), env)
  } else {
    qs <- lapply(syms, rlang::new_quosure, env = env)
    pred <- get_predicate(chk, env)
  }
  if (!nzchar(msg)) {
    msg <- attr(chk_eval, "def_err_msg", exact = TRUE) %||% ""
  }
  protect <- attr(chk_eval, "protect_msg", exact = TRUE) %||% FALSE
  text <- deparse_check(pred[["expr"]], qs, msg, protect, pred[["env"]])
  validation_tbl(pred[["fn"]], qs, text)
}

# stripped-down version of tibble:::list_to_tibble()
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
  is.call(x) && identical(x[[1]], as.name("quos"))
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
    fn <- try_eval_tidy(x, env)
    if (!is.function(fn)) {
      stop(err_not_function(x, fn), call. = FALSE)
    }
  }
  list(expr = expr, fn = fn, env = env)
}

is_lambda <- function(x) {
  is.call(x) && identical(x[[1]], as.symbol("{"))
}

err_not_function <- function(x, fault) {
  if (is_error(fault)) {
    sprintf("Error trying to determine whether %s is a function: %s",
            rlang::quo_text(x), conditionMessage(fault))
  } else {
    sprintf("Not a function: %s", rlang::quo_text(x))
  }
}

deparse_check <- function(expr, qs, def_msg, protect, env) {
  calls <- vapply(qs, deparse_call, character(1), expr = expr)
  msgs <- names_filled(qs)
  not_named <- !nzchar(msgs)
  msgs[not_named] <-
    generate_message(def_msg, protect, env, qs[not_named], calls[not_named])
  list(call = calls, msg = msgs, dot_as_expr = not_named)
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
#' @param q Quosure.
#' @param text Text to interpolate.
#' @return Glue object, i.e., string of class `glue`.
#' @examples
#' glue_opp("The length of {{sQuote(.)}} is {length(.)}.", q = rlang::quo(x))
#' # The length of ‘x’ is {length(.)}.
glue_opp <- function(q, text, env) {
  env_dot <- new.env(parent = env)
  env_dot[["."]] <- rlang::quo_text(q)
  glue_text(relevel_braces(text), env_dot)
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
  text_vec <- strsplit(text, NULL)[[1]]
  paste(relevel_braces_(text_vec), collapse = "")
}

relevel_braces_ <- function(x) {
  ht <- as.integer(cumsum(brace_val(x)))
  rle <- rle(ht != 0)
  out <- vector("list", length(rle[["values"]]))
  pos <- 0
  for (i in seq_along(rle[["values"]])) {
    l <- rle[["lengths"]][i]
    seq <- pos + seq_len(l)
    if (rle[["values"]][i]) {
      if (is_double_brace(ht[seq])) {
        out[[i]] <- x[seq][2:(l - 1)]
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
  if (length(ht) <= 2) {
    FALSE
  } else {
    sum(ht == 1) == 2
  }
}
