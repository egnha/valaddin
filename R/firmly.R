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
    validation_closure(f, chks, sig, arg[["nm"]], arg[["sym"]])
  }
}

try_predicate <- function(p) {
  force(p)
  function(x) p(tryCatch(x, error = function(e) NULL))
}
is_local <- try_predicate(rlang::is_formula)
is_quos  <- try_predicate(rlang::is_quosures)

parse_check <- function(chk, msg, syms) {
  env <- rlang::get_env(chk)
  if (is_local(chk_eval <- rlang::eval_tidy(chk, env = env))) {
    lhs <- rlang::f_lhs(chk_eval)
    if (rlang::is_quosure(lhs)) {
      chk <- lhs
    } else {
      chk <- rlang::new_quosure(rlang::quo_expr(lhs), env)
    }
    rhs <- rlang::f_rhs(chk_eval)
    qs <- if (is_quos(rhs_eval <- rlang::eval_tidy(rhs, env = env))) {
      rhs_eval
    } else {
      list(rlang::new_quosure(rhs, env))
    }
  } else {
    qs <- lapply(syms, rlang::new_quosure, env = env)
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
  body <- rlang::quo_expr(q)
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
#' @param q Quosure.
#' @param text Text to interpolate.
#' @return Glue object, i.e., string of class `glue`.
#' @examples
#' glue_opp("The length of {{sQuote(.)}} is {length(.)}.", q = rlang::quo(x))
#' # The length of ‘x’ is {length(.)}.
glue_opp <- function(q, text) {
  env <- new.env(parent = rlang::get_env(q))
  env[["."]] <- rlang::quo_text(q)

  # substitute string into call to avoid binding string to env,
  # which could clash with a name in an environment higher up
  eval(bquote(glue::glue(.(relevel_braces(text)), .envir = env)))
}

name_predicates <- function(preds, exprs) {
  paste0(safely_rename("pred", avoid = exprs), seq_along(preds))
}

safely_rename <- function(nm, avoid) {
  n <- max(unlist(lapply(avoid, function(expr) rapply(as.list(expr), nchar))))
  filler <- paste(character(n), collapse = "_")
  paste(nm, filler, sep = "_")
}

bind_predicates <- function(nms, preds) {
  env <- new.env(parent = emptyenv())
  for (i in seq_along(nms)) {
    assign(nms[i], rlang::eval_tidy(preds[[i]]), envir = env)
  }
  env
}

bind_promises <- function(nms, exprs, env_eval, parent) {
  env_assign <- new.env(parent = parent)
  for (i in seq_along(nms))
    eval(bquote(
      delayedAssign(.(nms[[i]]), .(exprs[[i]]), env_eval, env_assign)
    ))
  env_assign
}

express_check <- function(exprs, nms) {
  lapply(seq_along(exprs), function(i)
    list(
      expr = rlang::expr(UQ(as.name(nms[[i]]))(UQE(exprs[[i]]))),
      env  = rlang::get_env(exprs[[i]])
    )
  )
}

validation_closure <- function(f, chks, sig, nms, syms) {
  force(f)
  force(nms)
  force(syms)

  nms_pred <- name_predicates(chks[["pred"]], chks[["expr"]])
  env_pred <- bind_predicates(nms_pred, chks[["pred"]])
  make_promises <- eval(call("function", sig, quote(environment())))
  new_validation_env <- function(call, env) {
    env_prom <- eval(`[[<-`(call, 1L, make_promises), env)
    parent.env(env_prom) <- environment(f)
    bind_promises(nms, syms, env_prom, env_pred)
  }
  exprs <- express_check(chks[["expr"]], nms_pred)

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
      venv <- encl[["new_validation_env"]](call, parent.frame())
      verdict <- suppressWarnings(
        lapply(encl[["exprs"]], function(.)
          tryCatch({
            parent.env(encl[["env_pred"]]) <- .[["env"]]
            eval(.[["expr"]], venv)
          }, error = identity)
        )
      )
      pass <- vapply(verdict, isTRUE, logical(1))
      if (all(pass)) {
        eval(`[[<-`(call, 1L, encl[["f"]]), parent.frame())
      } else {
        fail <- !pass
        msg_call  <- encl[["deparse_w_defval"]](call)
        msg_error <- encl[["enumerate_many"]](
          encl[["problems"]](encl[["chks"]][fail, ], verdict[fail], venv)
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
      sprintf("Error evaluating check %s: %s",
              chks$call[[i]], x$message)
    } else {
      sprintf("Predicate value %s not TRUE/FALSE: %s",
              chks$call[[i]], deparse_collapse(x))
    }
  }, character(1))
}

error_message <- function(msg, call, q, env) {
  env_dot <- bind_as_dot(q, env)
  tryCatch(
    # substitute string into call to avoid binding string to env,
    # which could clash with a name in an environment higher up
    eval(bquote(glue::glue(.(msg), .envir = env_dot))),
    error = function(e) {
      sprintf("%s\n[Error interpolating message '%s': %s]",
              message_false(call), msg, conditionMessage(e))
    }
  )
}

bind_as_dot <- function(q, env) {
  parent.env(env) <- rlang::get_env(q)
  env_dot <- new.env(parent = env)
  eval(bquote(delayedAssign(".", .(rlang::quo_expr(q)), env, env_dot)))
  env_dot
}
