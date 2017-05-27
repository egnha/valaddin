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
  glue::glue(glue::glue(text, . = "{{.}}"), . = rlang::quo_text(q))
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

express_check <- function(exprs, nm_pred, nm_arg, nm_env) {
  sym_env <- lapply(nm_env, as.symbol)
  get_arg <- lapply(nm_arg, function(.)
    # use get0, instead of `[[`, because it raises missing-argument error
    rlang::expr(
      get0(UQ(.), envir = UQ(sym_env[["prom"]]))
    )
  )
  names(get_arg) <- nm_arg
  lapply(seq_along(exprs), function(i) {
    expr <- eval(rlang::expr(substitute(UQ(exprs[[i]]), get_arg)))
    expr <- rlang::expr(
      UQ(sym_env[["pred"]])[[UQ(nm_pred[[i]])]](UQE(expr))
    )
    list(
      expr = expr,
      env  = rlang::get_env(exprs[[i]])
    )
  })
}

bind_predicates <- function(preds, env) {
  u_preds <- unique(preds)
  nm_pred <- character(length(preds))
  for (i in seq_along(u_preds)) {
    nm <- sprintf("pred_%s", i)
    assign(nm, rlang::eval_tidy(u_preds[[i]]), envir = env)
    nm_pred[vapply(preds, identical, logical(1), y = u_preds[[i]])] <- nm
  }
  nm_pred
}

validation_closure <- function(f, chks, sig, nm_arg) {
  env_pred <- new.env(parent = emptyenv())

  nm_pred <- bind_predicates(chks[["pred"]], env_pred)
  nm_env  <- safely_rename("prom", "pred", avoid = chks[["expr"]])

  make_promises <- eval(call("function", sig, quote(environment())))
  ve <- new.env(parent = emptyenv())
  ve[[nm_env[["pred"]]]] <- env_pred
  new_validation_env <- function(call, env) {
    ve[[nm_env[["prom"]]]] <- eval(`[[<-`(call, 1L, make_promises), env)
    parent.env(ve[[nm_env[["prom"]]]]) <- environment(f)
    ve
  }

  exprs <- express_check(chks[["expr"]], nm_pred, nm_arg, nm_env)

  function() {
    mc <- match.call()
    encl <- parent.env(parent.frame())
    ve <- encl[["new_validation_env"]](mc, parent.frame())
    # Make version that catches the first error, like stopifnot()
    verdict <- suppressWarnings(
      lapply(encl[["exprs"]], function(.)
        tryCatch(eval(.[["expr"]], `parent.env<-`(ve, .[["env"]])),
                 error = identity)
      )
    )
    pass <- vapply(verdict, isTRUE, logical(1))

    if (all(pass)) {
      eval(`[[<-`(call, 1L, encl[["f"]]), parent.frame())
    } else {
      stop("!")
    }
  }
}

}

}

# msg <- "Not positive"
# a <- 1
# quos(
#   is.numeric,
#   quos(is.numeric, x, y),
#   !!msg := quos({. > 0}, x, !!local_msg := y - !!a),
#   "Error message" = quos(is.character, x, y, paste(z, !!a))
# )
