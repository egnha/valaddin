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

#' @importFrom rlang eval_bare
validation_closure <- function(f, chks, sig, nms, syms, error_class) {
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
  error <- function(call, verdict, fail, env) {
    err_call <- deparse_w_defval(call)
    err_msgs <- problems(chks[fail, ], verdict[fail], env)
    structure(
      list(
        message    = paste(err_call, enumerate_many(err_msgs), sep = "\n"),
        call       = NULL,
        match_call = call,
        error_call = chks[fail, ][["call"]],
        error_msgs = err_msgs
      ),
      class = c(error_class, "error", "condition")
    )
  }
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
      venv <- .subset2(encl, "new_validation_env")(call, parent.frame())
      verdict <- suppressWarnings(
        lapply(.subset2(encl, "exprs"), function(.) {
          parent.env(encl[["env_pred"]]) <- .subset2(., "env")
          tryCatch(eval_bare(.subset2(., "expr"), venv), error = identity)
        })
      )
      pass <- vapply(verdict, isTRUE, logical(1))
      if (all(pass)) {
        eval_bare(`[[<-`(call, 1L, .subset2(encl, "f")), parent.frame())
      } else {
        stop(.subset2(encl, "error")(call, verdict, !pass, venv))
      }
    }
  )
}

problems <- function(chks, verdict, env) {
  vapply(seq_along(verdict), function(i) {
    out <- verdict[[i]]
    if (is_false(out)) {
      err_invalid_input(chks[i, ], env)
    } else if (inherits(out, "error")) {
      err_eval_error(chks$call[[i]], out)
    } else {
      err_invalid_value(chks$call[[i]], out)
    }
  }, character(1))
}

err_invalid_input <- function(., env) {
  parent.env(env) <- rlang::get_env(.$expr[[1]])
  env_dot <- if (.$dot_as_expr[[1]]) bind_as_dot(.$expr[[1]], env) else env
  tryCatch(
    # substitute string into call to avoid binding string to env,
    # which could clash with a name in an environment higher up
    eval(bquote(glue::glue(.(.$msg[[1]]), .envir = env_dot))) %||%
      # work-around bug in glue 1.0.0 (get character(0) for certain strings)
      "",
    error = function(e) {
      sprintf("%s\n[Error interpolating message '%s': %s]",
              message_false(.$call[[1]]), .$msg[[1]], conditionMessage(e))
    }
  )
}

bind_as_dot <- function(q, env) {
  env_dot <- new.env(parent = env)
  eval(bquote(delayedAssign(".", .(rlang::quo_expr(q)), env, env_dot)))
  env_dot
}

err_eval_error <- function(call, out) {
  sprintf("Error evaluating check %s: %s", call, conditionMessage(out))
}

err_invalid_value <- function(call, out) {
  sprintf("Predicate value %s not TRUE/FALSE: %s", call, deparse_collapse(out))
}
