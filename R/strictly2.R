enumerate_chr <- function(x) {
  paste(
    purrr::map_chr(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]])),
    collapse = ""
  )
}

lazy_assign <- function(ldots, env) {
  for (x in ldots) {
    eval(substitute(
      delayedAssign(deparse(x$expr), ..expr.., x$env, env),
      list(..expr.. = x$expr)
    ))
  }
  invisible(env)
}

# Use warnings to signal missing arguments
validate <- function(calls, lzydots, parent = parent.frame()) {
  env <- lazy_assign(lzydots, new.env(parent = parent))
  is_ok <- purrr::map_lgl(seq_along(calls), function(i) {
    tryCatch(
      suppressWarnings(eval(calls[[i]], env)),
      error = function(e) {
        names(calls)[[i]] <<-
          sprintf("Error evaluating check `%s`: %s",
                  deparse_collapse(calls[[i]]), e$message)
        FALSE
      }
    )
  })
  list(errmsg = names(calls)[!is_ok], warn = NULL)
}

check_args2 <- function(calls, dots) {
  substitute({
    `_args` <- do.call(lazyeval::lazy_dots, ..dots..)
    `_res` <- validate(..calls.., `_args`)
    if (!is.null(`_res`$warn)) {
      warning(`_res`$warn, call. = FALSE)
    }
    if (length(`_res`$errmsg)) {
      `_call` <- paste0(deparse_collapse(match.call()), "\n")
      `_msg` <-  enumerate_chr(`_res`$errmsg)
      stop(paste0(`_call`, `_msg`), call. = FALSE)
    }
  }, list(..calls.. = calls, ..dots.. = dots))
}

strictly2_ <- function(.f, ..., .checklist = list(), .check_missing = FALSE) {
  sig <- formals(.f)
  args_fml <- lapply(setdiff(names(sig), "..."), as.name)
  chks <- do.call("c", c(list(...), .checklist))
  chk_args <- if (length(chks)) {
    calls <- do.call("c", lapply(chks, generate_calls))
    check_args2(calls, args_fml)
  } else {
    quote(NULL)
  }
  body_orig <- body(.f)
  body <- substitute({
    ..chk_args..
    ..body..
  }, list(..chk_args.. = chk_args, ..body.. = body_orig))

  f <- eval(call("function", sig, body))
  environment(f) <- environment(.f)

  class_f <- if (inherits(.f, "strict_closure")) NULL else "strict_closure"
  structure(
    f,
    ..body.. = body_orig,
    class = c(class_f, class(.f))
  )
}
