#' @include attributes.R
NULL

#' Apply a function strictly
#'
#' \code{strictly()} hardens a function with input validation, using formulae to
#' specify checks. It modifies a function to respond more consistently to
#' errors, and is therefore an adverb, like the purrr functions
#' \code{\link[purrr]{safely}()} and \code{\link[purrr]{possibly}()}. The
#' process of making a function strict can be undone using \code{nonstrictly()}.
#'
#' @name strictly
NULL

lazy_assign <- function(lzydots, env) {
  for (x in lzydots) {
    eval(substitute(
      delayedAssign(deparse(x$expr), ..expr.., x$env, env),
      list(..expr.. = x$expr)
    ))
  }
  invisible(env)
}

#' @export
validate <- function(calls, lazy_args, parent = parent.frame()) {
  if (!length(calls)) {
    character(0)
  } else {
    env <- lazy_assign(lazy_args, new.env(parent = parent))
    is_ok <- purrr::map_lgl(seq_along(calls), function(i) {
      tryCatch(
        suppressWarnings(eval(calls[[i]]$call, env)),
        error = function(e) {
          names(calls)[[i]] <<-
            sprintf("Error evaluating check `%s`: %s",
                    calls[[i]]$call_chr, e$message)
          FALSE
        }
      )
    })
    names(calls)[!is_ok]
  }
}

#' @export
invalid_input <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("invalid_input", "error", "condition")
  )
}

template <- function(calls, arg_fml, arg_req) {
  substitute({
    `_args` <- names(match.call(expand.dots = FALSE)[-1L])
    `_warn` <- setdiff(..arg_req.., `_args`)
    if (length(`_warn`)) {
      `_missing` <- paste(`_warn`, collapse = ", ")
      `_msg` <- sprintf("Missing required argument(s): %s", `_missing`)
      warning(`_msg`, call. = FALSE)
    }
    `_lazy_args` <- do.call(lazyeval::lazy_dots, ..dots..)
    `_fail` <- valaddin::validate(..calls.., `_lazy_args`)
    if (length(`_fail`)) {
      `_call` <- sprintf("%s\n", valaddin::deparse_collapse(match.call()))
      `_msg` <- paste0(`_call`, valaddin::enumerate_many(`_fail`))
      stop(valaddin::invalid_input(`_msg`))
    }
  }, list(..calls.. = calls, ..dots.. = arg_fml, ..arg_req.. = arg_req))
}

unfurl_args <- function(lhs, arg_nm, arg_symb, env) {
  q <- lapply(arg_symb, lazyeval::f_new, env = env)
  if (!is.null(lhs)) {
    names(q) <- paste(lhs, encodeString(arg_nm, quote = "`"), sep = ": ")
  } else {
    names(q) <- rep("", length(q))
  }
  q
}

generate_calls <- function(chk, arg_nm, arg_symb, env = lazyeval::f_env(chk)) {
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
    unfurl_args(lhs, arg_nm, arg_symb, env)
  }
  call_chr <- purrr::map_chr(q, function(.) {
    expr <- substitute(f(x), list(f = p_symb, x = lazyeval::f_rhs(.)))
    deparse_collapse(expr)
  })
  is_empty <- names(q) == ""
  names(q)[is_empty] <- sprintf("FALSE: %s", call_chr[is_empty])

  setNames(
    purrr::map2(q, call_chr, function(x, y) {
      list(call = as.call(c(predicate, lazyeval::f_rhs(x))), call_chr = y)
    }),
    names(q)
  )
}

strict_closure <- function(sig, arg_symb, body, env, attr, class,
                           calls, arg_req) {
  arg_fml <- if (length(calls)) arg_symb else list()
  chk_tmpl <- template(calls, arg_fml, arg_req)
  new_body <- substitute({
    ..checks..
    ..body..
  }, list(..checks.. = chk_tmpl, ..body.. = body))
  f <- eval(call("function", sig, new_body))
  environment(f) <- env
  attributes(f)  <- attr

  structure(
    f,
    ..sc_core..    = body,
    ..sc_check..   = calls,
    ..sc_arg_req.. = arg_req,
    class = prepend_to_vec("strict_closure", class)
  )
}

strictly_ <- function(.f, ..., .checklist = list(), .warn_missing = NULL) {
  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks; see '?valaddin::strictly'", call. = FALSE)
  }

  if (!length(chks) && is.null(.warn_missing)) {
    return(.f)
  }

  sig <- formals(.f)
  arg <- nomen(sig)
  body_orig <- sc_core(.f) %||% body(.f)
  calls <- c(
    sc_check(.f),
    do.call("c", lapply(chks, generate_calls, arg_nm   = arg$nm,
                                              arg_symb = arg$symb))
  )
  arg_req <- if (is.null(.warn_missing)) {
    sc_arg_req(.f)
  } else if (.warn_missing) {
    sc_arg_req(.f) %||% arg$nm[arg$wo_value]
  } else {
    NULL
  }

  strict_closure(
    sig, arg$symb, body_orig, environment(.f), attributes(.f), class(.f),
    calls, arg_req
  )
}

nonstrictly_ <- function(.f) {
  if (!inherits(.f, "strict_closure")) {
    .f
  } else {
    f <- eval(call("function", formals(.f), sc_core(.f)))
    environment(f) <- environment(.f)
    sc_attrs <- c("..sc_core..", "..sc_check..", "..sc_arg_req..")
    attributes(f) <- attributes(.f)[setdiff(names(attributes(.f)), sc_attrs)]
    class(f) <- class(.f)[class(.f) != "strict_closure"]
    f
  }
}

#' @section Specifying argument checks:
#'   An argument check is specified by a formula whose interpretation depends on
#'   whether it is one- or two-sided:
#'   \itemize{
#'     \item One-sided formula \code{~p}: reads "For every argument for which
#'       \code{p} fails, stop with a message of this failure." For example, the
#'       formula \code{~is.numeric} enforces a check for all arguments to be
#'       numerical.
#'     \item Two-sided formula \code{q ~ p}: reads "Stop with the message
#'       \code{q}, if the check \code{p} fails." For example, the "logicalness"
#'       of the \code{.chk_missing} argument of \code{strictly}, itself, is
#'       enforced by the formula \code{"Not logical: `.chk_missing`" ~
#'       purrr::is_scalar_logical(.chk_missing)}.
#'   }
#'
#'   More precisely, for a one-sided formula \code{~p}, the expression \code{p}
#'   is a predicate function which is applied to each argument of \code{.f}; for
#'   a two-side formula \code{q ~ p}, \code{q} is an expression whose evaluation
#'   yields a string---the "failure message"---while \code{p} is an expression
#'   that is a call to a predicate function---the "check". (In particular, the
#'   evaluation of \code{p} must always yield either \code{TRUE} or
#'   \code{FALSE}.) A message generated by a failing check becomes part of the
#'   total message of the condition \code{.cond}.
#'
#'   A two-sided formula check is most useful when one needs to exercise special
#'   control of a specific argument, or needs to express a more complicated
#'   check involving several arguments, e.g., \code{"Invalid range" ~ (0 < a) &&
#'   (a <= b) && (b <= c)}, specifies that (the arguments) \code{a}, \code{b},
#'   \code{c} must be a monotonically increasing sequence of positive numbers. A
#'   one-sided formula check is simply a handy shorthand for submitting each and
#'   every argument to a common check.
#' @param .f Interpreted function, i.e., closure (not a primitive function).
#' @param ... Check formulae.
#' @param .checklist List of check formulae.
#' @param .warn_missing Logical or \code{NULL}. Should the absence of required
#'   arguments be checked? ("Required" arguments are those without default
#'   value.) This question is disregarded when \code{.warn_missing} is
#'   \code{NULL}.
#' @examples
#' foo <- function(x, y, a = "sum:", ...) paste(a, x + y)
#' foo(1, 2)                     # "sum: 3"
#' foo(1, 2, "SUM:")             # "SUM: 3"
#' foo(1, 2, "SUM:", bogus_obj)  # "SUM: 3" (arguments are evaluated lazily)
#'
#' foo_strict <- strictly(foo)
#' identical(foo, foo_strict)
#'
#' foo_stricter <- strictly(foo_strict, list(~x, ~y) ~ is.numeric)
#' foo_stricter(1, 2)                     # "sum: 3"
#' foo_stricter(1, 2, "SUM:", bogus_obj)  # "SUM: 3"
#' foo_stricter(1, "2")                   # FALSE: is.numeric(y)
#' foo_stricter(1, 2, NA_real_)           # "NA 3"
#'
#' # `foo` and `foo_stricter` share the same environment
#' identical(environment(foo), environment(foo_stricter))
#'
#' # `foo_stricter` preserves the attributes of `foo`
#' all(attributes(foo) %in% attributes(foo_stricter))
#'
#' foo_strictest <- strictly(
#'   foo_stricter,
#'   list("Not string" ~ a) ~ purrr::is_scalar_character,
#'   .warn_missing = TRUE
#' )
#' foo_strictest(1, 2)            # "sum: 3"
#' foo_strictest(1, "2")          # FALSE: is.numeric(y)
#' foo_strictest(1, 2, NA_real_)  # Not string
#' foo_strictest(1, a = "foo")    # Error evaluating check, missing argument
#'
#' g <- function(x, y) x - y
#' g_sct <- strictly(bar, ~is.numeric, ~{. > 0})
#' g_sct(1, 2)        # -1
#' g_sct(1, -2)       # FALSE: (purrr::as_function(~{. > 0}))(y)
#' g_sct("1", 2)      # FALSE: is.numeric(x)
#' g_sct("1", "two")  # FALSE: is.numeric(x), is.numeric(y)
#' g_sct("1", -2)     # FALSE: is.numeric(x), (purrr::as_function(~{. > 0}))(y)
#'
#' h <- strictly(
#'   function(x, y) log(x - y),
#'   "Not numeric" ~ is.numeric,
#'   list(~y, "`x` not greater than `y`" ~ x - y) ~ {. > 0}
#' )
#' h(4, 2)    # 0.6931472
#' h(3, 0)    # FALSE: (purrr::as_function(~{. > 0}))(y)
#' h(3, 3)    # `x` not greater than `y`
#' h("4", 1)  # Not numeric: `x`, error evaluating check
#' @rdname strictly
#' @export
strictly <- strictly_(
  strictly_,
  list("`.f` not an interpreted function" ~ .f) ~
    purrr::is_function,
  list("`.f` has no explicit arguments (only '...')" ~ .f) ~
    {!identical(names(formals(.)), "...")},
  list("`.warn_missing` not NULL or logical" ~ .warn_missing) ~
    {is.null(.) || purrr::is_scalar_logical(.)},
  .warn_missing = TRUE
)

#' @rdname strictly
#' @export
nonstrictly <- strictly_(
  nonstrictly_,
  list("`.f` not an interpreted function" ~ .f) ~ purrr::is_function,
  .warn_missing = TRUE
)

#' @export
print.strict_closure <- function(x) {
  cat("<strict_closure>\n")

  cat("\n* Core:\n")
  cat(deparse(args(x))[[1L]], "\n", sep = "")
  print(sc_core(x))
  print(environment(x))

  cat("\n* Checks (<predicate> : <error message>):\n")
  chks <- sc_check(x)
  if (length(chks)) {
    labels <- paste0(
      purrr::map_chr(chks, "call_chr"), " :\n    \"", names(chks), "\""
    )
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }

  cat("\n* Check for missing arguments:\n")
  arg_req <- sc_arg_req(x)
  if (length(arg_req)) {
    cat(paste(arg_req, collapse = ", "))
  } else {
    cat("Not checked\n")
  }
}