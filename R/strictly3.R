is_true  <- isTRUE
is_false <- function(x) identical(FALSE, x)

report_error <- function(i, .expr, .msg, .string, .env) {
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
                                 .msg    = .calls$msg,
                                 .string = .calls$string,
                                 .env = env)
    is_problematic <- !is.na(.calls$msg)

    if (any(is_problematic)) {
      error <- .calls[is_problematic, ]
      list(result = NULL, error = error)
    } else {
      eval(.call_fn(call), parent, parent)
    }
  }
}

#' @export
strictly3_ <- function(.f, ..., .checklist = list(), .warn_missing = FALSE) {
  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks", call. = FALSE)
  }

  sig <- formals(.f)
  arg <- nomen(sig)
  call_fn <- caller(.f)
  maybe_warn <- if (.warn_missing) warn(arg$nm[arg$wo_value]) else invisible

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