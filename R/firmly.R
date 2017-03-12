#' @include functions.R components.R checklist.R future.R rawrd.R utils.R
NULL

# Checking infrastructure -------------------------------------------------

is_void_symb <- function(x) is.symbol(x) && x == substitute()

# Represent non-dot arguments by name and symbol (sig: pairlist)
nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(
    nm       = nm,
    symb     = lapply(nm, as.symbol),
    wo_value = vapply(sig[nm], is_void_symb, logical(1), USE.NAMES = FALSE)
  )
}

# Make a list of argument-expressions (as formulas), named by error message
unfurl_args <- function(.errmsg, .arg_nm, .arg_symb, .env) {
  q <- lapply(.arg_symb, ff_new, env = .env)
  names(q) <- if (is.null(.errmsg)) {
    character(length(q))
  } else {
    paste(.errmsg, .arg_nm, sep = ": ")
  }

  q
}

expr_lambda <- function(body) {
  call("function", as.pairlist(alist(. = )), body)
}

call_str <- function(chk_item, fn_expr) {
  call <- substitute(f(x), list(f = fn_expr, x = lazyeval::f_rhs(chk_item)))
  deparse_collapse(call)
}

# Expand a check formula into a data frame of checks
assemble <- function(.chk, .nm, .symb, .env = lazyeval::f_env(.chk)) {
  p <- lazyeval::f_rhs(.chk)
  p_expr <- if (is_lambda(p)) expr_lambda(p) else p
  predicate <- eval(p_expr, .env)

  lhs <- ff_eval_lhs(.chk)
  q <- if (is.list(lhs)) {
    # .chk: local scope
    do.call(lazyeval::f_list, lhs)
  } else {
    # .chk: global scope (lhs: string/NULL)
    unfurl_args(.errmsg = lhs, .nm, .symb, .env)
  }
  string <- vapply(q, call_str, FUN.VALUE = character(1L), fn_expr = p_expr)
  is_empty <- names(q) == ""
  names(q)[is_empty] <- sprintf("FALSE: %s", string[is_empty])

  dplyr::as_data_frame(
    list(
      expr   = lapply(q, function(.) as.call(c(predicate, lazyeval::f_rhs(.)))),
      env    = list(.env),
      string = string,
      msg    = names(q)
    ),
    validate = FALSE
  )
}

# Warning apparatus -------------------------------------------------------

warn <- function(.args) {
  if (!length(.args)) {
    return(NULL)
  }

  function(.call) {
    missing <- setdiff(.args, names(.call[-1L]))

    if (length(missing)) {
      msg <- paste(
        sprintf("Argument(s) expected but not specified in call %s:",
                deparse_collapse(.call)),
        quote_collapse(missing)
      )
      warning(msg, call. = FALSE)
    }

    invisible(.call)
  }
}

warning_closure <- function(.fn, .warn) {
  force(.fn)
  force(.warn)

  function() {
    call <- match.call()
    .warn(call)

    parent <- parent.frame()
    eval(.fn(call), parent, parent)
  }
}

# Validation apparatus ----------------------------------------------------

is_error <- function(x) inherits(x, "error")

problems <- function(chks, verdict) {
  vapply(seq_along(verdict), function(i) {
    x <- verdict[[i]]
    if (is_false(x)) {
      chks$msg[[i]]
    } else if (is_error(x)) {
      sprintf("Error evaluating check %s: %s", chks$string[[i]], x$message)
    } else {
      sprintf("Predicate value %s not TRUE/FALSE: %s",
              chks$string[[i]], deparse_collapse(x))
    }
  }, character(1))
}

validating_closure <- function(.chks, .sig, .fn, .warn) {
  force(.chks)
  force(.sig)
  force(.fn)
  force(.warn)

  function() {
    call <- match.call()
    .warn(call)

    parent <- parent.frame()
    encl <- parent.env(environment())
    env <- promises(call, encl$.sig, parent)
    verdict <- Map(
      function(expr, env_chk) {
        parent.env(env) <- env_chk
        tryCatch(
          suppressWarnings(eval(expr, env, env)),
          error = identity
        )
      },
      encl$.chks$expr, encl$.chks$env
    )
    pass <- vapply(verdict, is_true, logical(1))

    if (all(pass)) {
      eval(.fn(call), parent, parent)
    } else {
      fail <- !pass
      msg_call  <- sprintf("%s\n", deparse_collapse(call))
      msg_error <- enumerate_many(problems(encl$.chks[fail, ], verdict[fail]))
      stop(paste0(msg_call, msg_error), call. = FALSE)
    }
  }
}

# Functional operators ----------------------------------------------------

skip <- function(...) invisible()

firmly_ <- function(.f, ..., .checklist = list(), .warn_missing = character()) {
  chks <- unname(c(list(...), .checklist))
  if (!is_checklist(chks)) {
    stop("Invalid check formula(e)", call. = FALSE)
  }

  sig <- formals(.f)
  arg <- nomen(sig)

  reason_to_pass <- c(
    no_validation = !length(chks) && !length(.warn_missing),
    no_named_arg  = !length(arg$nm)
  )
  if (any(reason_to_pass)) {
    if (!reason_to_pass[["no_validation"]]) {
      warning("No input validation applied: `.f` has no named argument",
              call. = FALSE)
    }
    return(.f)
  }

  is_unknown_arg <- !(.warn_missing %in% arg$nm)
  if (any(is_unknown_arg)) {
    err_msg <- "Invalid `.warn_missing`: %s not argument(s) of `.f`"
    unknown_arg <- quote_collapse(.warn_missing[is_unknown_arg])
    stop(sprintf(err_msg, unknown_arg), call. = FALSE)
  }

  fn <- call_fn(if (is_firm(.f)) firm_core(.f) else .f)
  pre_chks <- firm_checks(.f)
  maybe_warn <- warn(.warn_missing) %||% warn(firm_args(.f)) %||% skip

  if (length(chks)) {
    assembled_chks <- dplyr::distinct_(
      dplyr::bind_rows(
        pre_chks,
        lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb)
      )
    )
    f <- validating_closure(assembled_chks, sig, fn, maybe_warn)
  } else {
    # .warn_missing not empty
    f <- if (is.null(pre_chks)) {
      warning_closure(fn, maybe_warn)
    } else {
      validating_closure(pre_chks, sig, fn, maybe_warn)
    }
  }

  firm_closure(with_sig(f, sig, .attrs = attributes(.f)))
}

#' @export
is_firm <- function(x) {
  purrr::is_function(x) && inherits(x, "firm_closure")
}

firm_closure <- function(.f) {
  if (!is_firm(.f)) {
    class(.f) <- c("firm_closure", class(.f))
  }
  .f
}

#' @export
firmly <- firmly_(
  firmly_,
  list("`.f` not an interpreted function" ~ .f) ~ purrr::is_function,
  list("`.checklist` not a list" ~ .checklist) ~ is.list,
  list("`.warn_missing` not a character vector" ~ .warn_missing) ~
    {is.character(.) && !anyNA(.)}
)

loosely_ <- function(.f, .quiet = FALSE) {
  if (is_firm(.f)) {
    firm_core(.f)
  } else {
    if (!.quiet) {
      warning("Argument not a firmly applied function", call. = FALSE)
    }
    .f
  }
}

#' @export
loosely <- firmly(
  loosely_,
  list("`.f` not an interpreted function" ~ .f) ~ purrr::is_function,
  list("`.quiet` not TRUE/FALSE" ~ .quiet) ~ {is_true(.) || is_false(.)}
)

#' @export
print.firm_closure <- function(x, ...) {
  cat("<firm_closure>\n")

  cat("\n* Core function:\n")
  print(firm_core(x))

  cat("\n* Checks (<predicate>:<error message>):\n")
  calls <- firm_checks(x)
  if (!is.null(calls) && nrow(calls)) {
    labels <- paste0(calls$string, ":\n", encodeString(calls$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }

  cat("\n* Check for missing arguments:\n")
  args <- firm_args(x)
  if (!is.null(args) && length(args)) {
    cat(paste(args, collapse = ", "), "\n")
  } else {
    cat("Not checked\n")
  }
}

#' Apply a function firmly
#'
#' \code{firmly} transforms a function into a function with input validation
#' checks. \code{loosely} undoes the application of \code{firmly}, by returning
#' the original function, without checks. \code{is_firm} is a predicate function
#' that checks whether an object is a firmly applied function, i.e., a function
#' created by \code{firmly}.
#'
#' @aliases firmly loosely is_firm
#' @evalRd rd_usage(c("firmly", "loosely", "is_firm"))
#'
#' @param .f Interpreted function, i.e., a function of type \code{"closure"}.
#' @param \dots Check formula(e) (see \emph{Details}).
#' @param .checklist List of check formulae. These are combined with check
#'   formulae provided via \code{\dots}.
#' @param .warn_missing \code{TRUE} or \code{FALSE}: Should the absence of
#'   required arguments be checked? (A \dQuote{required argument} is a (named)
#'   argument without default value.) This question is disregarded if
#'   \code{.warn_missing} is \code{NULL}.
#' @param .quiet \code{TRUE} or \code{FALSE}: Should a warning be signaled if
#'   \code{.f} is not a function created by \code{firmly}?
#' @param x Object to probe.
#'
#' @return
#'   \subsection{\code{firmly}}{
#'     \code{firmly} does nothing if it has nothing to do: \code{.f} is returned
#'     unaltered if \code{.f} has no named arguments to check (i.e., \code{.f}
#'     has argument signature \code{function()} or \code{function(...)}).
#'
#'     Otherwise, \code{firmly} returns a function that behaves
#'     \emph{identically} to \code{.f}, with the exception that it validates its
#'     inputs before being called on them. In particular, \code{firmly} respects
#'     lazy evaluation: if all checks pass, then when it comes to calling the
#'     underlying function, all arguments, including those that undergo checks,
#'     are still lazily evaluated. If any check fails, an error that tabulates
#'     every failing check is signaled.
#'
#'     Additionally, \code{firmly} preserves the argument signature of
#'     \code{.f}, along with its attributes. (Sole exception: the resulting
#'     class is \code{"firm_closure"}, which contains the class of \code{.f}.)
#'   }
#'
#'   \subsection{\code{loosely}}{
#'     \code{loosely} returns the original function, stripped of any input
#'     validation checks imposed by \code{firmly}.
#'   }
#'
#' @details
#'   An \strong{input validation check} is specified by a \strong{check
#'   formula}, a special \link[stats]{formula} of the form
#'   \preformatted{<scope> ~ <predicate>}
#'   where the right-hand side expresses \emph{what} to check, and the left-hand
#'   side expresses \emph{where} to check it.
#'
#'   The right-hand side \code{<predicate>} is a \strong{predicate} function,
#'   i.e, a one-argument function that returns either \code{TRUE} or
#'   \code{FALSE}. It is the condition to check/enforce. The left-hand side
#'   \code{<scope>} is an expression specifying what the condition is to be
#'   applied to: whether the condition is to be applied to all \emph{named}
#'   arguments of \code{.f} (the case of \dQuote{global scope}), or whether the
#'   condition is to be selectively applied to certain expressions of the
#'   arguments (the case of \dQuote{local scope}).
#'
#'   Thus according to \strong{scope}, there are two classes of check formulae:
#'   \itemize{
#'     \item \strong{Check formulae of global scope}
#'       \preformatted{<string> ~ <predicate>}
#'       \preformatted{~ <predicate>}
#'
#'     \item \strong{Check formulae of local scope}
#'       \preformatted{list(<check_item>, <check_item>, ...) ~ <predicate>}
#'   }
#'
#'   \subsection{Check formulae of global scope}{
#'     A \strong{global check formula} is a succinct way of enforcing the
#'     veracity of the function \code{<predicate>} upon every (named) argument
#'     of \code{.f}. Each argument for which \code{<predicate>}
#'     \emph{fails}—evaluates to \code{FALSE} or is itself not
#'     evaluable—produces an error message, which is auto-generated, unless a
#'     custom error message is supplied by specifying the string
#'     \code{<string>}.
#'
#'     \subsection{Example}{
#'       The condition that all (named) arguments of a function must be
#'       numerical can be enforced by the check formula
#'       \preformatted{~ is.numeric}
#'       or
#'       \preformatted{"Not numeric" ~ is.numeric}
#'       if the custom error message \code{"Not numeric"} is to be used (in lieu
#'       of an auto-generated error message).
#'     }
#'   }
#'
#'   \subsection{Check formulae of local scope}{
#'     A \strong{local check formula} imposes argument-specific conditions. Each
#'     \strong{check item} \code{<check_item>} is a formula of the form \code{~
#'     <expression>} (one-sided) or \code{<string> ~ <expression>}; it imposes
#'     the condition that the function \code{<predicate>} is \code{TRUE} for the
#'     expression \code{<expression>}. As for global check formulae, each check
#'     item for which \code{<predicate>} fails produces an error message, which
#'     is auto-generated, unless a custom error message is supplied by a string
#'     as part of the left-hand side of the check item (formula).
#'
#'     \subsection{Example}{
#'       The condition that \code{x} and \code{y} must differ for the function
#'       \code{function(x, y) \{1 / (x - y)\}} can be enforced by the local
#'       check formula
#'       \preformatted{list(~ x - y) ~ function(.) abs(.) > 0}
#'       or
#'       \preformatted{list("x, y must differ" ~ x - y) ~ function(.) abs(.) > 0}
#'       if the custom error message \code{"x, y must differ"} is to be used (in
#'       lieu of an auto-generated error message).
#'     }
#'   }
#'
#'   \subsection{Anonymous predicate functions}{
#'     Following the
#'     \href{https://cran.r-project.org/package=magrittr}{\pkg{magrittr}}
#'     package, an anonymous (predicate) function of a single argument \code{.}
#'     can be concisely expressed by enclosing the body of such a function
#'     within curly braces \code{\{ \}}.
#'
#'     \subsection{Example}{
#'       The (onsided, global) check formula
#'       \preformatted{~ {. > 0}}
#'       is equivalent to the check formula \code{~ function(.) {. > 0}}
#'     }
#'   }
#'
#' @seealso \link{scope-changing}, \link{checklist}, \link{components},
#'   \link{bare-type-checkers}, \link{scalar-type-checkers},
#'   \link{type-checkers}, \link{misc-checkers}
#'
#' @examples
#' \dontrun{
#'
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#'
#' # Ensure that `f` is a function
#' secant_firm <- firmly(secant, list("`f` not a function" ~ f) ~ is.function)
#' secant_firm(log, 1, .1)    # 0.9531018
#' secant_firm("log", 1, .1)  # Error: "`f` not a function"
#'
#' # Ensure that `x` and `dx` are numerical (possibly non-scalars)
#' secant_vec <- firmly(secant_firm, list(~x, ~dx) ~ is.numeric)
#' secant_vec(log, c(1, 2), .1)  # 0.9531018 0.4879016
#' secant_vec("log", 1, .1)      # Error: "`f` not a function" (as before)
#' secant_vec(log, "1", .1)      # Error: "FALSE: is.numeric(x)"
#' secant_vec("log", "1", .1)    # Two errors
#'
#' # Ensure that `dx` is a numerical scalar
#' secant_scalar <- firmly(secant_firm, list(~dx) ~ purrr::is_scalar_numeric)
#' secant_scalar(log, c(1, 2), .1)    # 0.9531018 0.4879016 (as before)
#' secant_scalar(log, 1, c(.1, .05))  # Error: "FALSE: purrr::is_scalar_numeric(dx)"
#' secant_scalar(log, 1, ".1" / 2)    # Error evaluating check
#'
#' # Use purrr::lift() for predicate functions with multi-argument dependencies
#' f <- function(f, l, r) secant(f, l, dx = r - l)
#' is_monotone <- function(x, y) y - x > 0
#' secant_right <- firmly(f, list(~list(l, r)) ~ purrr::lift(is_monotone))
#' secant_right(log, 1, 1.1)  # 0.9531018
#' secant_right(log, 1, .9)   # Error: "FALSE: purrr::lift(is_monotone)(list(l, r))"
#'
#' # Alternatively, secant_right() can be implemented with a unary check
#' secant_right2 <- firmly(f, list(~ r - l) ~ {. > 0})
#' all.equal(secant_right(log, 1, 1.1), secant_right2(log, 1, 1.1))  # TRUE
#' secant_right2(log, 1, .9)  # Error (as before)
#'
#' # firmly won't force any argument not involved in a check
#' g <- firmly(function(x, y) "Pass", list(~x) ~ is.character)
#' g(c("a", "b"), stop("Not signaled"))  # "Pass"
#'
#' # loosely recovers the underlying function
#' identical(loosely(secant_vec), secant)  # TRUE
#' }
#'
#' @name firmly
NULL
