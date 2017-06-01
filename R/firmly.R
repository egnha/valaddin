#' @importFrom rlang quos
#' @export
rlang::quos

#' @export
firmly <- function(f, ..., checklist = NULL) {
  vld(..., checklist = checklist)(f)
}

#' @export
`%checkin%` <- function(chks, f) {
  # identity function of rhs, provisionally
  f
}

nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, sym = lapply(nm, as.symbol))
}

#' @export
vld <- function(..., checklist = NULL) {
  chks <- c(rlang::quos(...), checklist)
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    msgs <- names(chks) %||% character(length(chks))
    chks <- do.call(
      "rbind",
      Map(function(., ..) parse_check(., .., arg[["sym"]]), chks, msgs)
    )
    validation_closure(f, chks, sig, arg[["nm"]])
  }
}

is_local <- rlang::is_formula
is_quos <- function(expr) {
  is.call(expr) && identical(expr[[1]], as.symbol("quos"))
}

parse_check <- function(chk, msg, syms) {
  chk_tidy <- rlang::eval_tidy(chk)
  if (is_local(chk_tidy)) {
    env <- rlang::get_env(chk)
    chk <- rlang::new_quosure(rlang::f_lhs(chk_tidy), env)
    rhs <- rlang::f_rhs(chk_tidy)
    qs <- if (is_quos(rhs)) {
      rlang::eval_tidy(rhs)
    } else {
      list(rlang::new_quosure(rhs, env))
    }
  } else {
    qs <- lapply(syms, rlang::new_quosure, env = baseenv())
  }
  pred <- lambda(chk)
  text <- deparse_check(pred, qs, msg)
  validation_df(pred, qs, text)
}

validation_df <- function(pred, exprs, text) {
  n <- length(exprs)
  d <- list(
    pred = `[<-`(vector("list", n), list(pred)),
    expr = exprs,
    call = text$call,
    msg  = text$msg
  )
  class(d) <- "data.frame"
  attr(d, "row.names") <- .set_row_names(n)
  d
}

# Variation of rlang::as_function
lambda <- function(q) {
  body <- rlang::get_expr(q)
  if (is_lambda(body)) {
    expr <- call("function", as.pairlist(alist(. = )), body)
    rlang::new_quosure(expr, rlang::get_env(q))
  } else {
    q
  }
}

deparse_check <- function(pred, qs, default) {
  calls <- vapply(qs, deparse_call, character(1), q = pred)
  msgs <- names(qs) %||% character(length(qs))
  not_named <- !nzchar(msgs)
  msgs[not_named] <- generate_message(default, qs[not_named], calls[not_named])
  list(call = calls, msg = msgs)
}

deparse_call <- function(q, arg) {
  call <- rlang::quo_expr(rlang::expr(UQE(q)(UQ(arg))))
  deparse_collapse(call)
}

generate_message <- function(default, qs, calls) {
  if (nzchar(default)) {
    vapply(qs, glue_opp, character(1), text = default)
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
#' @param dot Quosure or expression.
#' @param text Text to interpolate.
#' @return Glue object.
#' @examples
#' glue_opp("The length of {{sQuote(.)}} is {length(.)}.", dot = quote(x))
#' # The length of ‘x’ is {length(.)}.
glue_opp <- function(text, dot) {
  glue::glue(relevel_braces(text), . = rlang::quo_text(dot))
}

safely_rename <- function(..., avoid) {
  nms <- list(...)
  n <- max(unlist(lapply(avoid, function(expr) rapply(as.list(expr), nchar))))
  filler <- paste(character(n), collapse = "_")
  setNames(paste(nms, filler, sep = "_"), nms)
}

bind_predicates <- function(preds, nm, env) {
  names(preds) <- paste0(nm, seq_along(preds))
  for (n in names(preds)) {
    assign(n, rlang::eval_tidy(preds[[n]]), envir = env)
  }
  names(preds)
}

express_check <- function(exprs, nm_pred, nm_arg, nm_prom) {
  get_arg <- lapply(nm_arg, function(.)
    # use get0, instead of `[[`, because get0 raises missing-argument error
    rlang::expr(get0(UQ(.), envir = UQ(as.name(nm_prom))))
  )
  names(get_arg) <- nm_arg
  lapply(seq_along(exprs), function(i) {
    expr <- eval(rlang::expr(substitute(UQ(exprs[[i]]), get_arg)))
    expr <- rlang::expr(UQ(as.name(nm_pred[[i]]))(UQE(expr)))
    list(
      expr = expr,
      env  = rlang::get_env(exprs[[i]])
    )
  })
}

validation_closure <- function(f, chks, sig, nm_arg) {
  nm_safe <- safely_rename("prom", "pred", avoid = chks[["expr"]])

  ve <- new.env(parent = emptyenv())
  nm_pred <- bind_predicates(chks[["pred"]], nm_safe[["pred"]], ve)
  make_promises <- eval(call("function", sig, quote(environment())))
  new_validation_env <- function(call, env) {
    ve[[nm_safe[["prom"]]]] <- eval(`[[<-`(call, 1L, make_promises), env)
    parent.env(ve[[nm_safe[["prom"]]]]) <- environment(f)
    ve
  }

  exprs <- express_check(chks[["expr"]], nm_pred, nm_arg, nm_safe[["prom"]])

  # Local bindings to avoid (unlikely) clashes with formal arguments
  enumerate_many <- match.fun("enumerate_many")
  problems <- match.fun("problems")
  deparse_w_defval <- function(call) {
    sig[names(call[-1L])] <- call[-1L]
    sig <- sig[!vapply(sig, identical, logical(1), quote(expr = ))]
    deparse_collapse(as.call(c(call[[1L]], sig)))
  }

  `formals<-`(
    value = sig,
    function() {
      call <- match.call()
      encl <- parent.env(environment())
      ve <- encl[["new_validation_env"]](call, parent.frame())
      # Make version that catches the first error, like stopifnot()
      verdict <- suppressWarnings(
        lapply(encl[["exprs"]], function(.)
          tryCatch(
            eval(.[["expr"]], `parent.env<-`(ve, .[["env"]])),
            error = identity
          )
        )
      )
      pass <- vapply(verdict, isTRUE, logical(1))
      if (all(pass)) {
        eval(`[[<-`(call, 1L, encl[["f"]]), parent.frame())
      } else {
        fail <- !pass
        msg_call  <- encl[["deparse_w_defval"]](call)
        msg_error <- encl[["enumerate_many"]](
          encl[["problems"]](encl[["chks"]][fail, ], verdict[fail],
                               ve[[encl[["nm_safe"]][["prom"]]]])
        )
        stop(paste(msg_call, msg_error, sep = "\n"), call. = FALSE)
      }
    }
  )
}

problems <- function(chks, verdict, env) {
  vapply(seq_along(verdict), function(i) {
    x <- verdict[[i]]
    if (is_false(x)) {
      error_message(chks$msg[[i]], chks$call[[i]], chks$expr[[i]], env)
    } else if (inherits(x, "error")) {
      sprintf("Error evaluating check %s: %s", chks$call[[i]], x$message)
    } else {
      sprintf("Predicate value %s not TRUE/FALSE: %s",
              chks$call[[i]], deparse_collapse(x))
    }
  }, character(1))
}

error_message <- function(msg, call, q, env, fallback_msg) {
  env_msg <- new.env(parent = env)
  env_msg[["msg"]] <- msg
  tryCatch(
    {
      env[["."]] <- eval(rlang::get_expr(q), env)
      glue::glue(msg, .envir = env_msg)
    },
    error = function(e) {
      fallback_msg <- message_false(call)
      expr <- rlang::get_expr(q)
      sprintf("%s\n(Error interpolating message %s, with `.` = %s: %s)",
              fallback_msg, msg, expr, e[["message"]])
    }
  )
}
