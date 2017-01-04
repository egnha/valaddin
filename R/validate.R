#' @include monads.R
NULL

assemble <- function(chk, nm, symb, env = lazyeval::f_env(chk)) {
  p <- lazyeval::f_rhs(chk)
  if (is_lambda(p)) {
    predicate <- lambda(p, env)
    p_symb    <- substitute(purrr::as_function(~x), list(x = p))
  } else {
    predicate <- lazyeval::f_eval_rhs(chk)
    p_symb    <- p
  }

  lhs <- lazyeval::f_eval_lhs(chk)
  q <- if (is.list(lhs)) {
    do.call(lazyeval::f_list, lhs)
  } else {
    unfurl_args(lhs, nm, symb, env)
  }
  string <- purrr::map_chr(q, function(.) {
    expr <- substitute(f(x), list(f = p_symb, x = lazyeval::f_rhs(.)))
    deparse_collapse(expr)
  })
  is_empty <- names(q) == ""
  names(q)[is_empty] <- sprintf("FALSE: %s", string[is_empty])

  purrr::pmap_df(list(q, string, names(q)), function(x, s, msg) {
    dplyr::data_frame(
      expr   = list(as.call(c(predicate, lazyeval::f_rhs(x)))),
      string = s,
      msg    = msg
    )
  })
}

predval <- dplyr::data_frame(
  desc = c("NULL", "NA", "void", "not logical", "TRUE", "FALSE"),
  pred = list(
    purrr::is_null,
    function(.) purrr::is_vector(.) && is.na(.),
    function(.) identical(., logical(0)),
    Negate(purrr::is_scalar_logical),
    isTRUE,
    function(.) identical(., FALSE)
  ),
  value = c(rep(NA, 4), TRUE, FALSE)
)
predval$desc <- sprintf("Predicate value is %s: %%s", predval$desc)

# .msg_if_false must not contain "%s"
value_as_msg <- function(em, .expr, .string, .msg_if_false,
                         .predval = predval) {
  if (is.null(em$error)) {
    .predval$desc[length(.predval$desc)] <- .msg_if_false
    wh <- which_first_true(.predval$pred, em$value)
    msg   <- sprintf(.predval$desc[wh], .string)
    value <- .predval$value[wh]
  } else {
    msg   <- sprintf("Error evaluating check `%s`: %s", .string, em$error)
    value <- NA
  }

  dplyr::data_frame(
    expr   = list(.expr),
    string = .string,
    msg    = msg,
    value  = value
  )
}

eval_check_expr <- function(expr, string, msg, env) {
  force(env)

  eval_quietly <- function(.) suppressWarnings(eval(., env))
  res <- error_fmap(eval_quietly)(error_unit(expr))

  writer(value = env, log = value_as_msg(res, expr, string, msg))
}

checker <- function(expr, string, msg) {
  force(expr); force(string); force(msg)

  f <- writer_function(
   function(env) eval_check_expr(expr, string, msg, env)
  )

  function(wm) bind.writer_monad(wm, f)
}

pipeline <- function(.x, .fs, ...) {
  Reduce(function(x, f) f(x, ...), .fs, init = .x)
}

with_sig <- function(.f, sig, env = environment(.f)) {
  f <- eval(call("function", sig, body(.f)))
  environment(f) <- env
  f
}

validate2_ <- function(wm) {
  errors <- dplyr::bind_rows(wm$log) %>%
    `[`(is.na(.$value) | .$value != TRUE, )

  x <- if (nrow(errors)) {
    list(value = NULL, error = errors)
  } else {
    list(value = wm$value, error = NULL)
  }

  error(x)
}

call_with <- function(f, call) {
  call[[1L]] <- f
  call
}

proto_strictly <- function(.f, ..., .checklist, .process) {
  force(.process)

  chks <- c(list(...), .checklist)
  arg <- nomen(formals(.f))
  calls <- chks %>%
    lapply(assemble, nm = arg$nm, symb = arg$symb) %>%
    dplyr::bind_rows()
  check_inputs <- purrr::pmap(calls, checker)
  validate <- c(writer_unit, check_inputs, validate2_)

  f <- function(...) {
    env <- do.call(lazyeval::lazy_dots, arg$symb) %>%
      lazy_assign(env = new.env(parent = parent.frame()))
    call <- call_with(.f, match.call())
    eval_fn <- function(e) eval(call, e)

    .process(pipeline(env, c(validate, error_fmap(eval_fn))))
  }

  with_sig(f, sig = formals(.f))
}

strictly_with_process <- function(process) {
  force(process)
  function(.f, ..., .checklist = list()) {
    proto_strictly(.f, ..., .checklist = .checklist, .process = process)
  }
}

process_strictly <- function(em) {
  if (is.null(em$error)) {
    em$value
  } else {
    msg <- if (is.data.frame(em$error)) {
      enumerate_many(em$error$msg)
    } else {
      em$error
    }
    stop(msg, call. = FALSE)
  }
}

process_safely <- function(em) {
  class(em) <- NULL
  em
}

#' @export
strictly2_ <- strictly_with_process(process_strictly)

#' @export
safely_ <- strictly_with_process(process_safely)