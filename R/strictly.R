caller <- function(.f) {
  force(.f)

  function(.call) {
    .call[[1L]] <- .f
    .call
  }
}

with_sig <- function(.f, .sig, .env = environment(.f)) {
  f <- eval(call("function", .sig, body(.f)))
  environment(f) <- .env
  f
}

assemble <- function(.chk, .nm, .symb, .env = lazyeval::f_env(.chk)) {
  p <- lazyeval::f_rhs(.chk)
  if (is_lambda(p)) {
    predicate <- lambda(p, .env)
    p_symb    <- substitute(purrr::as_function(~x), list(x = p))
  } else {
    predicate <- lazyeval::f_eval_rhs(.chk)
    p_symb    <- p
  }

  lhs <- lazyeval::f_eval_lhs(.chk)
  q <- if (is.list(lhs)) {
    do.call(lazyeval::f_list, lhs)
  } else {
    unfurl_args(lhs, .nm, .symb, .env)
  }
  string <- purrr::map_chr(q, function(.) {
    expr <- substitute(f(x), list(f = p_symb, x = lazyeval::f_rhs(.)))
    deparse_collapse(expr)
  })
  is_empty <- names(q) == ""
  names(q)[is_empty] <- sprintf("FALSE: %s", string[is_empty])

  purrr::pmap_df(list(q, string, names(q)), function(x, s, m) {
    dplyr::data_frame(
      expr   = list(as.call(c(predicate, lazyeval::f_rhs(x)))),
      string = s,
      msg    = m
    )
  })
}

report_error <- function(i, .expr, .string, .msg, .env) {
  tryCatch(
    {
      val <- suppressWarnings(eval(.expr[[i]], .env, .env))

      if (is_true(val))
        NA_character_
      else if (is_false(val))
        .msg[[i]]
      else
        sprintf("Predicate value %s is invalid: %s",
                .string[[i]], deparse_collapse(val))
    },
    error = function(e)
      sprintf("Error evaluating check %s: %s", .string[[i]], e$message)
  )
}

warn <- function(.ref_args) {
  force(.ref_args)

  function(.call) {
    missing <- setdiff(.ref_args, names(.call[-1L]))

    if (length(missing)) {
      msg <- missing %>%
        paste(collapse = ", ") %>%
        sprintf("Missing required argument(s): %s", .)
      warning(msg, call. = FALSE)
    }

    invisible(.call)
  }
}

make_warning_closure <- function(.call_fn, .warn) {
  function() {
    call <- match.call()
    parent <- parent.frame()

    .warn(call)

    eval(.call_fn(call), parent, parent)
  }
}

make_strict_closure <- function(.calls, .args, .call_fn, .warn) {
  function() {
    call <- match.call()

    .warn(call)

    promises <- do.call(lazyeval::lazy_dots, .args)
    parent <- parent.frame()
    env <- lazy_assign(promises, new.env(parent = parent))

    .calls$msg <- purrr::map_chr(seq_len(nrow(.calls)), report_error,
                                 .expr   = .calls$expr,
                                 .string = .calls$string,
                                 .msg    = .calls$msg, .env = env)
    is_problematic <- !is.na(.calls$msg)

    if (any(is_problematic)) {
      error <- .calls[is_problematic, ]
      stop(enumerate_many(error$msg), call. = FALSE)
    } else {
      eval(.call_fn(call), parent, parent)
    }
  }
}

strictly_ <- function(.f, ..., .checklist = list(), .warn_missing = FALSE) {
  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks", call. = FALSE)
  }

  sig <- formals(.f)
  arg <- nomen(sig)
  maybe_warn <- if (.warn_missing) warn(arg$nm[arg$wo_value]) else invisible
  call_fn <- caller(.f)

  f <- if (!length(chks)) {
    make_warning_closure(call_fn, maybe_warn)
  } else {
    calls <- dplyr::bind_rows(
      lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb)
    )
    make_strict_closure(calls, arg$symb, call_fn, maybe_warn)
  }

  with_sig(f, sig)
}

checks <- list(
  list("`.f` not an interpreted function" ~ .f) ~
    purrr::is_function,
  list("`.warn_missing` not a logical scalar" ~ .warn_missing) ~
    {purrr::is_scalar_logical(.) && !purrr::is_empty(.)}
)

#' @export
strictly <- strictly_(strictly_, .checklist = checks, .warn_missing = TRUE)