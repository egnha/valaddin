# Variation of rlang::as_function
lambda <- function(q) {
  bdy <- rlang::get_expr(q)
  if (is_lambda(bdy)) {
    expr <- call("function", as.pairlist(alist(.x = , .y = , . = .x)), bdy)
    rlang::new_quosure(expr, rlang::get_env(q))
  } else {
    q
  }
}

q_deparse_call <- function(q, args) {
  rlang::quo_text(rlang::expr(UQE(q)(UQ(args))))
}

default_msg <- function(q, exprs, default) {
  if (nzchar(default)) {
    text <- vapply(exprs, rlang::quo_text, character(1))
    paste(default, encodeString(text, quote = "`"), sep = ": ")
  } else {
    text <- vapply(exprs, q_deparse_call, character(1), q = q)
    ws <- ifelse(grepl("\n", text), "\n", " ")
    paste0("FALSE:", ws, text)
  }
}

validation_df <- function(q, exprs, msgs) {
  n <- length(exprs)
  d <- list(
    pred = `[<-`(vector("list", n), list(q)),
    expr = exprs,
    msg  = msgs
  )
  class(d) <- "data.frame"
  attr(d, "row.names") <- .set_row_names(n)
  d
}

parse_check <- function(quo_chk, syms) {
  q <- lambda(quo_chk[[1]])

  if (length(quo_chk) == 1) {
    exprs <- lapply(syms, rlang::new_quosure, env = emptyenv())
  } else {
    exprs <- quo_chk[-1]
  }

  msgs <- names(exprs) %||% character(length(exprs))
  not_named <- !nzchar(msgs)
  msgs[not_named] <- default_msg(q, exprs[not_named], names(quo_chk)[1])

  validation_df(q, exprs, msgs)
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
      get0(UQ(.), envir = UQ(sym_env[["prom"]]), inherits = FALSE)
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

vld_ <- function(..., checklist = NULL) {
  quo_chks <- c(rlang::quos(...), checklist)
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    chks <- lapply(quo_chks, assemble_checks, nm = arg$nm, symb = arg$symb)
    validate(f, chks, sig, arg$nm)
  }
}

q_firmly <- function(f, ..., checklist = NULL) {
  vld_(..., checklist = checklist)(f)
}

# msg <- "Not positive"
# a <- 1
# quos(
#   is.numeric,
#   quos(is.numeric, x, y),
#   !!msg := quos({. > 0}, x, !!local_msg := y - !!a),
#   "Error message" = quos(is.character, x, y, paste(z, !!a))
# )
