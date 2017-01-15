#' @include utils.R
NULL

unfurl_args <- function(.lhs, .arg_nm, .arg_symb, .env) {
  q <- lapply(.arg_symb, lazyeval::f_new, env = .env)
  if (!is.null(.lhs)) {
    names(q) <- paste(.lhs, encodeString(.arg_nm, quote = "`"), sep = ": ")
  } else {
    names(q) <- rep("", length(q))
  }

  q
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

warning_closure <- function(.fn, .warn) {
  function() {
    call <- match.call()
    parent <- parent.frame()

    .warn(call)

    eval(.fn(call), parent, parent)
  }
}

report_error <- function(.expr, .string, .msg, .env) {
  tryCatch(
    {
      val <- suppressWarnings(eval(.expr, .env, .env))

      if (is_true(val))
        NA_character_
      else if (is_false(val))
        .msg
      else
        sprintf("Predicate value %s neither TRUE nor FALSE: %s",
                .string, deparse_collapse(val))
    },
    error = function(e)
      sprintf("Error evaluating check %s: %s", .string, e$message)
  )
}

validating_closure <- function(.chks, .args, .fn, .warn) {
  function() {
    call <- match.call()

    .warn(call)

    promises <- do.call(lazyeval::lazy_dots, .args)
    parent <- parent.frame()
    env <- lazy_assign(promises, new.env(parent = parent))

    # unlist(Map()) is somewhat faster than purrr::pmap_chr()
    .chks$msg <- unlist(Map(function(e, s, m) report_error(e, s, m, env),
                            .chks$expr, .chks$string, .chks$msg))
    is_problematic <- !is.na(.chks$msg)

    if (any(is_problematic)) {
      msg_call  <- sprintf("%s\n", deparse_collapse(call))
      msg_error <- enumerate_many(.chks[is_problematic, ]$msg)
      stop(paste0(msg_call, msg_error), call. = FALSE)
    } else {
      eval(.fn(call), parent, parent)
    }
  }
}

strict_closure <- function(.f) {
  structure(.f, class = c("strict_closure", class(.f)))
}

strictly_ <- function(.f, ..., .checklist = list(), .warn_missing = FALSE) {
  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks", call. = FALSE)
  }

  sig <- formals(.f)
  arg <- nomen(sig)
  maybe_warn <- if (.warn_missing) warn(arg$nm[arg$wo_value]) else invisible
  fn <- call_fn(.f)

  f <- if (!length(chks)) {
    warning_closure(fn, maybe_warn)
  } else {
    assembled_chks <- dplyr::bind_rows(
      lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb)
    )
    validating_closure(assembled_chks, arg$symb, fn, maybe_warn)
  }

  strict_closure(with_sig(f, sig))
}

checks <- list(
  list("`.f` not an interpreted function" ~ .f) ~
    purrr::is_function,
  list("`.warn_missing` not a logical scalar" ~ .warn_missing) ~
    {purrr::is_scalar_logical(.) && !purrr::is_empty(.)}
)

#' @export
strictly <- strictly_(strictly_, .checklist = checks, .warn_missing = TRUE)

#' @export
nonstrictly <- function(..f) {
  if (!is_strict_closure(..f)) {
    stop("Argument not a strictly applied function", call. = FALSE)
  }

  strict_core(..f)
}

#' @export
print.strict_closure <- function(x) {
  cat("<strict_closure>\n")

  cat("\n* Core function:\n")
  print(strict_core(x))

  cat("\n* Checks (<predicate>:<error message>):\n")
  calls <- strict_checks(x)
  if (!is.null(calls) && nrow(calls)) {
    labels <- paste0(calls$string, ":\n", encodeString(calls$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }

  cat("\n* Check for missing arguments:\n")
  args <- strict_args(x)
  if (!is.null(args) && length(args)) {
    cat(paste(args, collapse = ", "))
  } else {
    cat("Not checked\n")
  }
}
