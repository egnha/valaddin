#' @include promises.R functions.R components.R checklist.R future.R rawrd.R utils.R
NULL

unfurl_args <- function(.lhs, .arg_nm, .arg_symb, .env) {
  q <- lapply(.arg_symb, f_new, env = .env)
  if (!is.null(.lhs)) {
    names(q) <- paste(.lhs, encodeString(.arg_nm, quote = "`"), sep = ": ")
  } else {
    names(q) <- character(length(q))
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

assemble <- function(.chk, .nm, .symb, .env = lazyeval::f_env(.chk)) {
  p <- lazyeval::f_rhs(.chk)
  p_expr <- if (is_lambda(p)) expr_lambda(p) else p
  predicate <- eval(p_expr, .env)

  lhs <- lazyeval::f_eval_lhs(.chk)
  q <- if (is.list(lhs)) {
    do.call(lazyeval::f_list, lhs)
  } else {  # lhs: string or NULL
    unfurl_args(lhs, .nm, .symb, .env)
  }
  string <- vapply(q, call_str, FUN.VALUE = character(1), fn_expr = p_expr)
  is_empty <- names(q) == ""
  names(q)[is_empty] <- sprintf("FALSE: %s", string[is_empty])

  purrr::pmap_df(list(q, string, names(q)), function(x, s, m) {
    dplyr::data_frame(
      expr   = list(as.call(c(predicate, lazyeval::f_rhs(x)))),
      env    = list(.env),
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
      msg <- paste("Missing required argument(s):",
                   paste(missing, collapse = ", "))
      warning(msg, call. = FALSE)
    }

    invisible(.call)
  }
}

warning_closure <- function(.fn, .warn) {
  function() {
    call <- match.call()
    .warn(call)

    parent <- parent.frame()
    eval(.fn(call), parent, parent)
  }
}

problems <- function(chks, verdict) {
  vapply(seq_along(verdict), function(i) {
    x <- verdict[[i]]
    if (is_false(x))
      chks$msg[[i]]
    else if (is_error(x))
      sprintf("Error evaluating check %s: %s", chks$string[[i]], x$message)
    else
      sprintf("Predicate value %s not TRUE/FALSE: %s",
              chks$string[[i]], deparse_collapse(x))
  }, character(1))
}

validating_closure <- function(.chks, .sig, .fn, .warn) {
  function() {
    call <- match.call()

    # Warn about missing required arguments (if requested)
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

is_void_symb <- function(x) {
  is.symbol(x) && x == substitute()
}

# Represent non-dot arguments by name and symbol (sig: pairlist)
nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(
    nm       = nm,
    symb     = lapply(nm, as.symbol),
    wo_value = vapply(sig[nm], is_void_symb, logical(1), USE.NAMES = FALSE)
  )
}

strict_closure <- function(.f) {
  structure(.f, class = c("strict_closure", class(.f)))
}

#' @export
is_strict_closure <- function(x) {
  purrr::is_function(x) && inherits(x, "strict_closure")
}

strictly_ <- function(.f, ..., .checklist = list(), .warn_missing = NULL) {
  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks (see ?strictly)", call. = FALSE)
  }

  sig <- formals(.f)
  arg <- nomen(sig)

  is_some_check <-
    length(chks) && length(arg$nm) ||
    !is.null(.warn_missing) && any(arg$wo_value)
  if (!is_some_check) {
    return(.f)
  }

  is_missing <-
    is_true(.warn_missing) ||
    is.null(.warn_missing) && !is.null(strict_args(.f))
  maybe_warn <- if (is_missing) warn(arg$nm[arg$wo_value]) else skip
  f_core <- if (is_strict_closure(.f)) strict_core(.f) else .f
  fn <- call_fn(f_core)
  pre_chks <- strict_checks(.f)

  if (!length(chks)) {  # .warn_missing is either TRUE or FALSE
    if (is.null(pre_chks) && is_false(.warn_missing))
      return(f_core)

    f <- if (is.null(pre_chks))
      warning_closure(fn, maybe_warn)
    else
      validating_closure(pre_chks, sig, fn, maybe_warn)
  } else {
    assembled_chks <- dplyr::distinct(
      dplyr::bind_rows(pre_chks,
                       lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb))
    )
    f <- validating_closure(assembled_chks, sig, fn, maybe_warn)
  }

  strict_closure(with_sig(f, sig, .attrs = attributes(.f)))
}

#' @export
strictly <- strictly_(
  strictly_,
  list("`.f` not an interpreted function" ~ .f) ~ purrr::is_function,
  list("`.warn_missing` neither NULL nor logical scalar" ~ .warn_missing) ~
    {is.null(.) || purrr::is_scalar_logical(.) && !is.na(.)},
  .warn_missing = TRUE
)

nonstrictly_ <- function(.f, .quiet = FALSE) {
  if (is_strict_closure(.f)) {
    strict_core(.f)
  } else {
    if (!.quiet) {
      warning("Argument not a strictly applied function", call. = FALSE)
    }
    .f
  }
}

#' @export
nonstrictly <- strictly(
  nonstrictly_,
  list("`.f` not an interpreted function" ~ .f) ~ purrr::is_function,
  list("`.quiet` not TRUE/FALSE" ~ .quiet) ~ {is_true(.) || is_false(.)}
)

#' @export
print.strict_closure <- function(x, ...) {
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
    cat(paste(args, collapse = ", "), "\n")
  } else {
    cat("Not checked\n")
  }
}

#' Apply a function strictly
#'
#' \code{strictly()} transforms a function into a function with input validation
#' checks. \code{nonstrictly()} undoes the application of \code{strictly()}, by
#' returning the original function, without checks. \code{is_strict_closure()}
#' is a predicate function that checks whether an object is a function created
#' by \code{strictly()}.
#'
#' @evalRd rd_usage(c("strictly", "nonstrictly", "is_strict_closure"))
#' @evalRd rd_alias(c("strictly", "nonstrictly", "is_strict_closure"))
#'
#' @param .f Interpreted function, i.e., function of type \code{"closure"}, not
#'   a primitive function.
#' @param ... Check formula(e); see the section \dQuote{Check formulae.}
#' @param .checklist List of check formulae. Check formulae in \code{.checklist}
#'   are combined with check formulae provided via the \code{...} argument.
#' @param .warn_missing \code{TRUE} or \code{FALSE}: Should the absence of
#'   required arguments be checked? (A \dQuote{required argument} is a (named)
#'   argument without default value.) This question is disregarded if
#'   \code{.warn_missing} is \code{NULL}.
#' @param .quiet \code{TRUE} or \code{FALSE}: Should a warning be signaled if
#'   \code{.f} is not a function created by \code{strictly()}?
#' @param x Object to check.
#'
#' @return
#'   \subsection{\code{strictly()}}{
#'   If neither the check formulae nor the switch \code{.warn_missing} are
#'   applicable to \code{.f}, then \code{strictly()} simply returns \code{.f}.
#'   This is the case when \code{.f} has no named arguments, i.e., \code{.f} has
#'   argument signature \code{function()} or \code{function(...)}.
#'
#'   Otherwise, \code{strictly()} returns a function of class
#'   \code{"strict_closure"}. This function behaves \emph{identically} to
#'   \code{.f}, with the exception that it does input validation, as follows:
#'   \enumerate{
#'     \item Validation: Before \code{.f} is called, every check-formula
#'     predicate is evaluated: for each named argument, in the case of a global
#'     check formula, and for each check-item expression, in the case of a local
#'     check formula. Every resulting value of \code{FALSE}, or failure to
#'     evalute the predicate itself, is tabulated; these are the
#'     \emph{validation errors}.
#'
#'     \item Error reporting: If there are any validation errors, an error is
#'     signaled listing them. Execution halts.
#'
#'     \item Checks passed: If there are no validation errors, \code{.f} is
#'     called on the supplied arguments.
#'   }
#'   \code{strictly()} preserves the argument signature of \code{.f}, along with
#'   all its attributes (with the execption that the resulting class is
#'   \code{"strict_closure"}, which inherits from the class of \code{.f}).
#'   }
#'
#' @return \subsection{\code{nonstrictly()}}{
#'   Returns the original function without checks. (This works even if the
#'   original function has been removed.)}
#'
#' @section Check formulae:
#'   An input validation check is specified by a \link[stats]{formula}, where
#'   the right-hand side expresses \emph{what} to check, and the left-hand side
#'   expresses \emph{where} to check it:
#'   \preformatted{
#'   <scope> ~ <predicate>}
#'   \code{<predicate>} is a predicate function, i.e, a one-variable function
#'   that returns \code{TRUE} or \code{FALSE}. \code{<scope>} is an expression
#'   specifying whether the scope of \code{<predicate>} is \emph{global}—applies
#'   to all (named) arguments of \code{.f}—or \emph{local}—applies to certain
#'   arguments, either individually or in combination. Thus, with respect to
#'   \code{<scope>}, there are two classes of check formulae:
#'   \itemize{
#'     \item \strong{Check formulae with global scope}:\cr
#'     \code{~ <predicate>} (onsided) or \code{<string> ~ <predicate>}
#'
#'     \item \strong{Check formulae with local scope}:\cr
#'     \code{list(<check_item>, <check_item>, ...) ~ <predicate>}
#'   }
#'   \subsection{Check formulae with global scope}{
#'   A \emph{global check formula} asserts that the evaluation of
#'   \code{<predicate>} is \code{TRUE} for each (named) argument of \code{.f}.
#'   Each argument for which the \code{<predicate>} fails (i.e., evaluates to
#'   \code{FALSE}) produces an error message, which is auto-generated unless a
#'   custom error message is supplied by specifying the string \code{<string>}.
#'   \cr\cr
#'   Example: the assertion that all (named) arguments of a function must be
#'   numerical can be enforced by the check formula
#'   \preformatted{
#'   ~ is.numeric}
#'   or
#'   \preformatted{
#'   "Not numeric" ~ is.numeric}
#'   if the custom error message \code{"Not numeric"} is to be used.}
#'
#'   \subsection{Check formulae with local scope}{
#'   A \emph{local check formula} makes argument-specific assertions. Each
#'   \dQuote{check item} \code{<check_item>} is a formula of the form \code{~
#'   <expression>} (one-sided) or \code{<string> ~ <expression>}; it makes the
#'   assertion that the \code{<predicate>} evaluates to \code{TRUE} for the
#'   expression \code{<expression>}. As for global check formulae, each check
#'   item for which the \code{<predicate>} fails produces an error message,
#'   which is auto-generated unless a custom error message is supplied by a
#'   string as part of the left-hand side of the check item (formula).
#'   \cr\cr
#'   Example: the assertion that \code{x} and \code{y} must differ for the
#'   function \code{function(x, y) \{1 / (x - y)\}} can be enforced by the local
#'   check formulae
#'   \preformatted{
#'   list(~ x - y) ~ function(.) abs(.) > 0}
#'   or
#'   \preformatted{
#'   list("`x`, `y` must differ" ~ x - y) ~ function(.) abs(.) > 0}
#'   if the custom error message \code{"`x`, `y` must differ"} is to be used
#'   (in lieu of an auto-generated error message).}
#'
#' @section Anonymous predicate functions:
#'   Following the \link[magrittr]{magrittr} package, an anonymous (predicate)
#'   function of a single argument \code{.} can be specified by placing the body
#'   of such a function within curly braces \code{\{ \ldots \}}.
#'   \cr\cr
#'   For example, the (onsided, global) check formula
#'   \preformatted{
#'   ~ function(.) {. > 0}}
#'   is equivalent to the check formula \code{~ {. > 0}}.
#'
#' @seealso \link{checklist} — predicate functions for verifying the syntactic
#'   validity of checklists; \link{scope} — functions for converting the scope
#'   of check formulae (i.e., making reusable check formulae); \link{components}
#'   — functions for extracting components of a strictly applied function.
#'
#' @examples
#' \dontrun{
#'
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#'
#' # Ensure that `f` is a function
#' secant_stc <- strictly(secant, list("`f` not a function" ~ f) ~ is.function)
#' secant_stc(log, 1, .1)    # 0.9531018
#' secant_stc("log", 1, .1)  # Error: "`f` not a function"
#'
#' # Ensure that `x` and `dx` are numerical (possibly non-scalars)
#' secant_vec <- strictly(secant_stc, list(~x, ~dx) ~ is.numeric)
#' secant_vec(log, c(1, 2), .1)  # 0.9531018 0.4879016
#' secant_vec("log", 1, .1)      # Error: "`f` not a function" (as before)
#' secant_vec(log, "1", .1)      # Error: "FALSE: is.numeric(x)"
#' secant_vec("log", "1", .1)    # Two errors
#'
#' # Ensure that `dx` is a numerical scalar
#' secant_scalar <- strictly(secant_stc, list(~dx) ~ purrr::is_scalar_numeric)
#' secant_scalar(log, c(1, 2), .1)    # 0.9531018 0.4879016 (as before)
#' secant_scalar(log, 1, c(.1, .05))  # Error: "FALSE: purrr::is_scalar_numeric(dx)"
#' secant_scalar(log, 1, ".1" / 2)    # Error evaluating check
#'
#' # Use purrr::lift() for predicate functions with multi-argument dependencies
#' f <- function(f, l, r) secant(f, l, dx = r - l)
#' is_monotone <- function(x, y) y - x > 0
#' secant_right <- strictly(f, list(~list(l, r)) ~ purrr::lift(is_monotone))
#' secant_right(log, 1, 1.1)  # 0.9531018
#' secant_right(log, 1, .9)   # Error: "FALSE: purrr::lift(is_monotone)(list(l, r))"
#'
#' # Alternatively, secant_right() can be implemented with a unary check
#' secant_right2 <- strictly(f, list(~ r - l) ~ {. > 0})
#' all.equal(secant_right(log, 1, 1.1), secant_right2(log, 1, 1.1))  # TRUE
#' secant_right2(log, 1, .9)  # Error (as before)
#'
#' # strictly() won't force any argument not involved in a check
#' g <- strictly(function(x, y) "Pass", list(~x) ~ is.character)
#' g(c("a", "b"), stop("Not signaled"))  # "Pass"
#'
#' # nonstrictly() recovers the underlying function
#' identical(nonstrictly(secant_vec), secant)  # TRUE
#' }
#'
#' @name strictly
NULL
