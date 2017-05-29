#' @importFrom rlang quos
#' @export
rlang::quos

#' @export
q_firmly <- function(f, ..., checklist = NULL) {
  vld(..., checklist = checklist)(f)
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
      Map(function(., ..) parse_check(., .., arg$symb), chks, msgs)
    )
    validation_closure(f, chks, sig, arg$nm)
  }
}

parse_check <- function(chk, msg, syms) {
  chk_ <- rlang::eval_tidy(chk)
  is_local <- rlang::is_quosures(chk_)
  if (is_local) {
    pred <- lambda(chk_[[1L]])
    qs <- chk_[-1L]
  } else {
    pred <- lambda(chk)
    qs <- lapply(syms, rlang::new_quosure, env = baseenv())
  }
  text <- deparse_check(pred, qs, msg)
  validation_df(pred, qs, text)
}

# Variation of rlang::as_function
lambda <- function(q) {
  body <- rlang::get_expr(q)
  if (is_lambda(body)) {
    expr <- call("function", as.pairlist(alist(.x = , .y = , . = .x)), body)
    rlang::new_quosure(expr, rlang::get_env(q))
  } else {
    q
  }
}

deparse_check <- function(pred, qs, msg) {
  calls <- vapply(qs, q_deparse_call, character(1), q = pred)
  msgs <- names(qs) %||% character(length(qs))
  not_named <- !nzchar(msgs)
  msgs[not_named] <- default_msg(msg, qs[not_named], calls[not_named])
  list(call = calls, msg = msgs)
}

q_deparse_call <- function(q, arg) {
  rlang::quo_text(rlang::expr(UQE(q)(UQ(arg))))
}

default_msg <- function(msg, qs, calls) {
  if (nzchar(msg)) {
    vapply(qs, glue_esc_dot, character(1), text = msg)
  } else {
    message_false(calls)
  }
}

#' Glue-interpolate escaped dots
#'
#' @param q Quosure.
#' @param text Text to interpolate.
#' @return Glue object.
#' @examples
#' glue_esc_dot("The value of '{{.}}' is {.}", rlang::quo(x))
#' #> The value of 'x' is {.}
#' @noRd
glue_esc_dot <- function(text, q) {
  glue::glue(relevel_braces(text), . = rlang::quo_text(q))
}

message_false <- function(calls) {
  ws <- ifelse(grepl("\n", calls), "\n", " ")
  paste0("FALSE:", ws, calls)
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
  q_problems <- match.fun("q_problems")
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
          encl[["q_problems"]](encl[["chks"]][fail, ], verdict[fail],
                               ve[[encl[["nm_safe"]][["prom"]]]])
        )
        stop(paste(msg_call, msg_error, sep = "\n"), call. = FALSE)
      }
    }
  )
}

q_problems <- function(chks, verdict, env) {
  vapply(seq_along(verdict), function(i) {
    x <- verdict[[i]]
    if (is_false(x)) {
      error_message(chks$msg[[i]], chks$expr[[i]], env,
                    message_false(chks$call[[i]]))
    } else if (inherits(x, "error")) {
      sprintf("Error evaluating check %s: %s", chks$call[[i]], x$message)
    } else {
      sprintf("Predicate value %s not TRUE/FALSE: %s",
              chks$call[[i]], deparse_collapse(x))
    }
  }, character(1))
}

error_message <- function(msg, quo, env, fallback_msg) {
  env_msg <- new.env(parent = env)
  env_msg[["msg"]] <- msg
  tryCatch({
    env[["."]] <- capture_expr(rlang::get_expr(quo), env)
    glue::glue(msg, .envir = env_msg)
  }, error = function(e) fallback_msg)
}

capture_expr <- function(expr, env) {
  deparse_collapse(eval(expr, env))
}
