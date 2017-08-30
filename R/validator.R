validation_closure <- function(f, chks, sig, args, error_class) {
  force(f)
  force(error_class)

  nms_pred <- name_predicates(seq_along(chks$pred), chks$expr)
  env_pred <- bind_predicates(nms_pred, chks$pred)
  make_promises <-
    eval_bare(call("function", sig, quote(environment())), environment(f))
  new_validation_env <- function(call, env) {
    env_prom <- eval_bare(`[[<-`(call, 1, make_promises), env)
    bind_promises(args, env_prom, env_pred)
  }
  exprs <- express_check(chks$expr, nms_pred)
  error <- function(call, verdict, fail, env) {
    err_call <- deparse_w_defval(call)
    err_msgs <- problems(chks[fail, ], verdict[fail], env)
    structure(
      list(
        message    = paste(err_call, enumerate_many(err_msgs), sep = "\n"),
        call       = NULL,
        match_call = call,
        error_call = chks[fail, ]$call,
        error_msgs = err_msgs
      ),
      class = c(error_class, "error", "condition")
    )
  }
  deparse_w_defval <- function(call) {
    sig[names(call[-1])] <- call[-1]
    sig <- sig[!vapply(sig, identical, logical(1), quote(expr = ))]
    deparse_collapse(as.call(c(call[[1]], sig)))
  }

  function() {
    call <- sys.call()
    encl <- parent.env(environment())
    venv <- .subset2(encl, "new_validation_env")(call, parent.frame())
    verdict <- suppressWarnings(
      lapply(.subset2(encl, "exprs"), function(.) {
        parent.env(encl[["env_pred"]]) <- .subset2(., "env")
        tryCatch(eval_bare(.subset2(., "expr"), venv), error = identity)
      })
    )
    pass <- vapply(verdict, isTRUE, logical(1))
    if (all(pass))
      eval(`[[<-`(call, 1, .subset2(encl, "f")), parent.frame())
    else
      stop(.subset2(encl, "error")(match.call(), verdict, !pass, venv))
  }
}

name_predicates <- function(suffixes, exprs) {
  paste0(safely_rename("pred", avoid = exprs), suffixes)
}
safely_rename <- function(nm, avoid) {
  n <- max(unlist(lapply(avoid, function(expr) rapply(as.list(expr), nchar))))
  filler <- paste(character(n), collapse = "_")
  paste(nm, filler, sep = "_")
}
bind_predicates <- function(nms, preds) {
  env <- new.env(parent = emptyenv())
  for (i in seq_along(nms)) {
    assign(nms[i], preds[[i]], envir = env)
  }
  env
}
bind_promises <- function(args, env_eval, parent) {
  nms <- names(args)
  env_assign <- new.env(parent = parent)
  for (i in seq_along(nms))
    eval_bare(bquote(
      delayedAssign(.(nms[[i]]), .(args[[i]]), env_eval, env_assign)
    ))
  env_assign
}
express_check <- function(exprs, nms) {
  lapply(seq_along(exprs), function(i)
    list(
      expr = expr(UQ(as.name(nms[[i]]))(UQE(exprs[[i]]))),
      env  = get_env(exprs[[i]])
    )
  )
}

problems <- function(chks, verdict, env) {
  vapply(seq_along(verdict), function(i) {
    out <- verdict[[i]]
    if (is_false(out))
      err_invalid_input(chks[i, ], env)
    else if (is_error(out))
      err_eval_error(chks$call[[i]], out)
    else
      err_invalid_value(chks$call[[i]], out)
  }, character(1))
}
err_invalid_input <- function(., env) {
  parent.env(env) <- .$env_msg[[1]]
  env_msg <- if (.$is_msg_gbl[[1]]) bind_as_dot(.$expr[[1]], env) else env
  errmsg <- tryCatch(
    glue_text(.$msg[[1]], env_msg),
    error = function(e)
      err_msg_error(.$call[[1]], .$msg[[1]], conditionMessage(e))
  )
  l <- length(errmsg)
  if (l == 1) {
    errmsg
  } else {
    not_string <- sprintf("not a string (has length %d)", l)
    err_msg_error(.$call[[1]], .$msg[[1]], not_string)
  }
}
bind_as_dot <- function(q, env) {
  env_dot <- new.env(parent = env)
  eval_bare(bquote(delayedAssign(".", .(quo_expr(q)), env, env_dot)))
  env_dot
}
err_msg_error <- function(call, msg, err) {
  sprintf('%s\n[Error interpolating message "%s": %s]',
          message_false(call), msg, err)
}
err_eval_error <- function(call, out) {
  sprintf("Error evaluating check %s: %s", call, conditionMessage(out))
}
err_invalid_value <- function(call, out) {
  sprintf("Predicate value %s not TRUE/FALSE: %s", call, deparse_collapse(out))
}
