#' @include checklist.R
NULL

#' Add input validation to a function
#'
#' A common form of boilerplate code at the top of functions is argument
#' checking: You make some checks on the arguments, signal a condition if any
#' show-stopping checks fail, then move on to the meat of the function if
#' everything is good. The problem with this approach is that it can clutter up
#' the main work of a function with admin---it spoils the "fun" of a function
#' with the inconvenience of a security check. The function \code{strictly()}
#' alleviates this nuissance by allowing you to enhance an existing function
#' with input validation, by using formulas to specify the checks.
#'
#' @examples
#' foo <- function(x, y, a = "sum:", ...) paste(a, x + y)
#' foo(1, 2)                     # "sum: 3"
#' foo(1, 2, "SUM:")             # "SUM: 3"
#' foo(1, 2, "SUM:", bogus_obj)  # "SUM: 3" (arguments are evaluated lazily)
#'
#' foo_strict <- strictly(foo)
#' foo_strict(1, 2)                     # "sum: 3"
#' foo_strict(1, 2, "SUM:")             # "SUM: 3"
#' foo_strict(1, 2, "SUM:", bogus_obj)  # "SUM: 3"
#'
#' # `foo` and `foo_strict` share the same environment
#' identical(environment(foo), environment(foo_strict))
#'
#' foo_stricter <- strictly(foo_strict, list(~x, ~y) ~ is.numeric)
#' foo_stricter(1, 2)                     # "sum: 3"
#' foo_stricter(1, 2, "SUM:", bogus_obj)  # "SUM: 3"
#' foo_stricter(1, "2")                   # Error: is.numeric(y) is not TRUE
#' foo_stricter(1, 2, NA_real_)           # "NA 3"
#'
#' foo_strictest <- strictly(
#'   foo_stricter,
#'   list("Not string" ~ a) ~ purrr::is_scalar_character,
#'   .warn_missing = TRUE
#' )
#' foo_strictest(1, 2)            # "sum: 3"
#' foo_strictest(1, "2")          # Error: is.numeric(y) is not TRUE
#' foo_strictest(1, 2, NA_real_)  # Error: Not string
#' foo_strictest(1, a = "foo")    # Error and warning
#'
#' bar <- function(x, y) x - y
#' bar_strict <- strictly(bar, ~is.numeric, ~ ~{. >= 0})  # Double '~, indeed'
#' bar_strict(1, 2)        # -1
#' bar_strict(1, -2)       # is_positive(.) is FALSE: `y`
#' bar_strict("1", 2)      # is.numeric(.) is FALSE: `x`
#' bar_strict("1", "two")  # is.numeric(.) is FALSE: `x`, `y`
#'
#' # Multiple failures are captured in the error message
#' bar_strict("1", -2)
#' # is.numeric(.) is FALSE: `x`; is_positive(.) is FALSE: `y`
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
validate <- function(calls, lazy_args, args, req_args,
                     parent = parent.frame()) {
  if (!length(calls)) {
    err_msgs <- character(0)
  } else {
    env <- lazy_assign(lazy_args, new.env(parent = parent))
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
    err_msgs  <- names(calls)[!is_ok]
  }
  warn_msgs <- setdiff(req_args, args)
  list(error = err_msgs, warning = warn_msgs)
}

#' @export
invalid_input <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("invalid_input", "error", "condition")
  )
}

check_args <- function(calls, dots, req_args) {
  substitute({
    `_lazy_args` <- do.call(lazyeval::lazy_dots, ..dots..)
    `_args` <- names(match.call(expand.dots = FALSE)[-1L])
    `_res` <- valaddin::validate(..calls.., `_lazy_args`, `_args`, ..req_args..)
    if (length(`_res`$warning)) {
      `_missing` <- paste(`_res`$warning, collapse = ", ")
      `_msg` <- sprintf("Missing required argument(s): %s", `_missing`)
      warning(`_msg`, call. = FALSE)
    }
    if (length(`_res`$error)) {
      `_call` <- sprintf("%s\n", valaddin::deparse_collapse(match.call()))
      `_msg` <- paste0(`_call`, valaddin::enumerate_many(`_res`$error))
      stop(valaddin::invalid_input(`_msg`))
    }
  }, list(..calls.. = calls, ..dots.. = dots, ..req_args.. = req_args))
}

args_wo_defltval <- function(sig) {
  if (is.null(sig)) {
    return(character(0))
  }
  args <- sig[setdiff(names(sig), "...")]
  no_defltval <- purrr::map_lgl(args, function(.) {
    is.symbol(.) && as.character(.) == ""
  })
  names(args)[no_defltval]
}

expand_args <- function(lhs, sig, env) {
  arg_name <- setdiff(names(sig), "...")
  arg_symb <- lapply(arg_name, as.symbol)
  q <- lapply(arg_symb, lazyeval::f_new, env = env)
  names(q) <- if (!is.null(lhs)) {
    paste(lhs, encodeString(arg_name, quote = "`"), sep = ": ")
  } else {
    rep("", length(q))
  }
  q
}

# f: list("message" ~ arg, ...) ~ function
generate_calls <- function(chk, sig, env = lazyeval::f_env(chk)) {
  p <- lazyeval::f_rhs(chk)
  if (is_lambda(p)) {
    predicate <- lambda(p, env)
    p_symb <- predicate
  } else {
    predicate <- lazyeval::f_eval_rhs(chk)
    p_symb <- p
  }

  lhs <- lazyeval::f_eval_lhs(chk)
  q <- if (is.list(lhs)) {
    do.call(lazyeval::f_list, lhs)
  } else {
    expand_args(lhs, sig, env)
  }

  is_empty <- names(q) == ""
  names(q)[is_empty] <- purrr::map_chr(q[is_empty], function(.) {
    call_expr <- substitute(f(x), list(f = p_symb, x = lazyeval::f_rhs(.)))
    sprintf("FALSE: %s", deparse_collapse(call_expr))
  })

  lapply(q, function(.) as.call(c(predicate, lazyeval::f_rhs(.))))
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
  calls <- unlist(c(strict_check(.f), lapply(chks, generate_calls, sig = sig)))
  fml_args <- if (length(calls)) {
    lapply(setdiff(names(sig), "..."), as.name)
  } else {
    list()
  }
  req_args <- if (is.null(.warn_missing)) {
    strict_reqarg(.f)
  } else if (.warn_missing) {
    strict_reqarg(.f) %||% args_wo_defltval(sig)
  } else {
    NULL
  }
  chk_args <- check_args(calls, fml_args, req_args)
  body_orig <- strict_body(.f) %||% body(.f)
  body <- substitute({
    ..checks..
    ..body..
  }, list(..checks.. = chk_args, ..body.. = body_orig))

  f <- eval(call("function", sig, body))
  environment(f) <- environment(.f)
  attributes(f) <- attributes(.f)

  new_class <- if (inherits(.f, "strict_closure")) NULL else "strict_closure"
  structure(
    f,
    ..sc_body..     = body_orig,
    ..sc_chks..     = calls,
    ..sc_req_args.. = req_args,
    class = c(new_class, class(.f))
  )
}

nonstrictly_ <- function(..f) {
  if (!inherits(..f, "strict_closure")) {
    ..f
  } else {
    f <- eval(call("function", formals(..f), strict_body(..f)))
    environment(f) <- environment(..f)
    sc_attrs <- c("..sc_body..", "..sc_chks..", "..sc_req_args..")
    attributes(f) <- attributes(..f)[setdiff(names(attributes(..f)), sc_attrs)]
    class(f) <- class(..f)[class(..f) != "strict_closure"]
    f
  }
}

#' Create an object of class "strict_closure"
#'
#' @param x R object.
#' @keywords internal
strict_closure <- function(x, ...) {
  structure(x, class = c("strict_closure", class(x)), ...)
}

#' @rdname strictly
#' @param x R object.
#' @export
is_strict_closure <- function(x) {
  purrr::is_function(x) && inherits(x, "strict_closure")
}

get_attribute <- function(.attr) {
  force(.attr)
  function(x) attr(x, .attr, exact = TRUE)
}

#' @rdname strictly
#' @section Attributes of a strict closure:
#'   A strict closure stores properties of the function it "strictifies" as
#'   attributes: the function body, the checks, the condition, and the required
#'   arguments (if the strict closure is mandated to check their absence). The
#'   functions \code{strict_*()} are helper functions to extract these
#'   attributes.
#'
#'   The predicate function \code{is_strict_closure()} is a reasonably stringent
#'   test of whether an object is a value of the function \code{strictly()}.
#' @export
strict_body <- get_attribute("..sc_body..")

#' @rdname strictly
#' @export
strict_check <- get_attribute("..sc_chks..")

#' @rdname strictly
#' @export
strict_reqarg <- get_attribute("..sc_req_args..")

#' @rdname strictly
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
#' @param .f Interpreted function (i.e., closure), not a primitive function.
#' @param ... Formulae, or single list thereof, that specify the argument checks
#'   for \code{.f}.
#' @param .cond Condition object (class \code{"condition"}) to be signaled if
#'   any of the checks fail. If \code{.cond} is \code{NULL}, the default error
#'   handler is used.
#' @param .chk_missing Logical. Should we check whether any required arguments
#'   are missing? ("Required" arguments are explicit arguments without default
#'   values; since they are promises, they need not be evaluated in the function
#'   body.)
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
#' @param ..f Strict function, i.e., function of class \code{"strict_closure"}.
#' @export
nonstrictly <- strictly_(
  nonstrictly_,
  list("`..f` not an interpreted function" ~ ..f) ~ purrr::is_function,
  .warn_missing = TRUE
)

#' @export
print.strict_closure <- function(x) {
  cat("<strict_closure>\n")

  cat("\n* Body:\n")
  cat(deparse(args(x))[[1L]], "\n", sep = "")
  print(strict_body(x))
  print(environment(x))

  cat("\n* Checks (<predicate> : <error message>):\n")
  chks <- strict_check(x)
  if (length(chks)) {
    labels <- purrr::map2_chr(chks, names(chks), function(p, msg) {
      paste0("`", deparse_collapse(p), "`", " : \"", msg, "\"")
    })
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }

  cat("\n* Check missing:\n")
  req_arg <- strict_reqarg(x)
  if (length(req_arg)) {
    cat(paste(req_arg, collapse = ", "))
  } else {
    cat("Not checked\n")
  }
}