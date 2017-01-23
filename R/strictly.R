#' @include utils.R
NULL

unfurl_args <- function(.lhs, .arg_nm, .arg_symb, .env) {
  q <- lapply(.arg_symb, lazyeval::f_new, env = .env)
  if (!is.null(.lhs)) {
    names(q) <- paste(.lhs, encodeString(.arg_nm, quote = "`"), sep = ": ")
  } else {
    names(q) <- character(length(q))
  }

  q
}

lambda <- function(p, env) {
  purrr::as_function(lazyeval::f_new(p, env = env))
}

expr_lambda <- function(body) {
  call("function", pairlist(. = substitute()), body)
}

assemble <- function(.chk, .nm, .symb, .env = lazyeval::f_env(.chk)) {
  p <- lazyeval::f_rhs(.chk)
  if (is_lambda(p)) {
    predicate <- lambda(p, .env)
    p_symb    <- expr_lambda(p)
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
  string <- purrr::map_chr(q, function(.)
    sprintf(string_funexpr(p_symb), deparse_collapse(lazyeval::f_rhs(.)))
  )
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

is_void_symb <- function(x) {
  is.symbol(x) && x == substitute()
}

#' Represent non-dot arguments by name and symbol
#'
#' @param sig Pairlist.
#' @return List with components \code{"nm"} (character), \code{"symb"} (symbol),
#'   \code{"wo_value"} (logical).
#' @keywords internal
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

checks <- list(
  list("`.f` not an interpreted function" ~ .f) ~
    purrr::is_function,
  list("`.warn_missing` neither NULL nor logical scalar" ~ .warn_missing) ~
  {is.null(.) || purrr::is_scalar_logical(.) && !is.na(.)}
)

strictly_ <- function(.f, ..., .checklist = list(), .warn_missing = NULL) {
  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks", call. = FALSE)
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
  maybe_warn <- if (is_missing) warn(arg$nm[arg$wo_value]) else invisible
  f_core <- if (is_strict_closure(.f)) strict_core(.f) else .f
  fn <- call_fn(f_core)
  pre_chks <- strict_checks(.f)

  if (!length(chks)) {  # .warn_missing is either TRUE or FALSE
    if (is.null(pre_chks) && is_false(.warn_missing))
      return(f_core)

    f <- if (is.null(pre_chks))
      warning_closure(fn, maybe_warn)
    else
      validating_closure(pre_chks, arg$symb, fn, maybe_warn)
  } else {
    assembled_chks <- dplyr::distinct(
      dplyr::bind_rows(pre_chks,
                       lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb))
    )
    f <- validating_closure(assembled_chks, arg$symb, fn, maybe_warn)
  }

  strict_closure(with_sig(f, sig, .attrs = attributes(.f)))
}

#' Apply a function strictly
#'
#' \code{strictly()} transforms a function to a function with input validation
#' checks. \code{nonstrictly()} undoes the application of \code{strictly()}, by
#' returning the original function, without checks.
#'
#' @param .f Interpreted function, i.e., function of type \code{"closure"}, not
#'   a primitive function.
#'
#' @seealso \code{\link{checklist}}, \code{\link{components}}
#'
#' @name strictly
NULL

#' @rdname strictly
#' @export
#'
#' @param ... Check formula(e); see "Details".
#' @param .checklist List of check formulae.
#' @param .warn_missing \code{TRUE} or \code{FALSE}: Should the absence of
#'   required arguments be checked? (A "required argument" is a (named) argument
#'   without default value.) This question is disregarded if
#'   \code{.warn_missing} is \code{NULL}.
#'
#' @return \strong{\code{strictly()}} — If neither the check formulae nor the
#'   switch \code{.warn_missing} are applicable to \code{.f}, then
#'   \code{strictly()} simply returns \code{.f}. This is the case when \code{.f}
#'   has no named arguments, i.e., \code{.f} has argument signature
#'   \code{function()} or \code{function(...)}.
#'
#'   Otherwise, \code{strictly()} returns a function of class
#'   \code{"strict_closure"}. This function behaves \emph{identically} to
#'   \code{.f}, with the exception that it does input validation, as follows:
#'   \itemize{
#'     \item 1. Validation: Before \code{.f} is called, every check-formula
#'     predicate is evaluated: for each named argument, in the case of a global
#'     check formula, and for each check-item expression, in the case of a local
#'     check formula. Every resulting value of \code{FALSE}, or failure to
#'     evalute the predicate itself, is tabulated; these are the
#'     \emph{validation errors}.
#'
#'     \item 2. Error reporting: If there are any validation errors, an error is
#'     signaled listing them. Execution halts.
#'
#'     \item 3. Checks passed: If there are no validation errors, \code{.f} is
#'     called on the supplied arguments.
#'   }
#'
#'   \code{strictly()} preserves the argument signature of \code{.f}, along with
#'   all its attributes (with the execption that the resulting class is
#'   \code{"strict_closure"}, which inherits from the class of \code{.f}).
#'
#'   \emph{Technical note}: \code{strictly()} parsimoniously preserves the lazy
#'   nature of function arguments, when possible. In particular, if all checks
#'   pass, then any argument that is not involved in a check is passed to
#'   \code{.f} as an unevaluated promise.
#' @details
#'   \strong{Check formulae} — Input validation checks are specified by formulae
#'   conforming to one of two types:
#'   \itemize{
#'     \item \strong{Global check formulae}:\cr
#'     \code{~ <predicate>} (one-sided),\cr
#'     \code{<string> ~ <predicate>}
#'
#'     \item \strong{Local check formulae}:\cr
#'     \code{list(<check_item>, <check_item>, ...) ~ <predicate>}
#'   }
#'   where \code{<predicate>} is a predicate function, i.e., a unary function
#'   that returns either \code{TRUE} or \code{FALSE}.
#'
#'   A \emph{global check formula} asserts that the evaluation of
#'   \code{<predicate>} is \code{TRUE} for each (named) argument of \code{.f}.
#'   Each argument for which the \code{<predicate>} fails (i.e., evaluates to
#'   \code{FALSE}) produces an error message, which is auto-generated unless a
#'   custom error message is supplied by specifying the string \code{<string>}.
#'   For example, the assertion that all (named) arguments of a function must be
#'   numerical can be enforced by the check formula \code{~ is.numeric}, or
#'   \code{"Not numeric" ~ is.numeric}, if the custom error message \code{"Not
#'   numeric"} is to be used.
#'
#'   A \emph{local check formula} makes argument-specific assertions. Each
#'   "check item" \code{<check_item>} is a formula of the form \code{~
#'   <expression>} (one-sided) or \code{<string> ~ <expression>}; it makes the
#'   assertion that the \code{<predicate>} evaluates to \code{TRUE} for the
#'   expression \code{<expression>}. As for global check formulae, each check
#'   item for which the \code{<predicate>} fails produces an error message,
#'   which is auto-generated unless a custom error message is supplied by a
#'   string as part of the left-hand side of the check item (formula). For
#'   example, the assertion that \code{x} and \code{y} must differ for the
#'   function \code{function(x, y) 1 / (x - y)} can be enforced by the local
#'   check formula \code{list(~ x - y) ~ function(.) abs(.) > 0}, or
#'   \code{list("x, y must differ" ~ x - y) ~ function(.) abs(.) > 0}, if the
#'   custom error message \code{"x, y must differ"} is to be used.
#'
#'   Check formulae that are individually specified via the \code{...} argument
#'   of \code{strictly()} are combined with check formulae provided via the
#'   list-argument \code{.checklist}.
#'
#'   \strong{Anonymous predicate functions} — Following the
#'   \link[magrittr]{magrittr} package, an anonymous (predicate) function of a
#'   single argument \code{.} can be specified by placing the body of such a
#'   function within curly braces \code{\{ \dots \}}. For example, the (onsided,
#'   global) check formula \code{~function(.) {. > 0}} is equivalent to the
#'   check formula \code{~{. > 0}}.
#'
#' @examples
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#'
#' # Ensure that `f` is a function
#' secant_stc <- strictly(secant, list("`f` not a function" ~ f) ~ is.function)
#' secant_stc(log, 1, .1)    # 0.9531018
#' secant_stc("log", 1, .1)  # Error: "`f` not a function"
#'
#' # Ensure that `x` and `dx` are numerical (possibly vectors)
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
#' # Alternatively, secant_right() can be implement with a unary check
#' secant_right2 <- strictly(f, list(~ r - l) ~ {. > 0})
#' all.equal(secant_right(log, 1, 1.1), secant_right2(log, 1, 1.1))  # TRUE
#' secant_right2(log, 1, .9)  # Error (as before)
strictly <- strictly_(strictly_, .checklist = checks, .warn_missing = TRUE)

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

#' @rdname strictly
#' @export
#'
#' @param quiet \code{TRUE} or \code{FALSE}: Should a warning be signaled if
#'   \code{.f} is not a function created by \code{strictly()}?
#'
#' @return \strong{\code{nonstrictly()}} — Returns the original function without
#'   checks.
#'
#' @examples
#'
#' # nonstrictly() recovers the underlying function
#' identical(nonstrictly(secant_vec), secant)  # TRUE
nonstrictly <- strictly_(
  nonstrictly_,
  list("Argument not an interpreted function" ~ .f) ~ purrr::is_function,
  list("`.quiet` is not TRUE/FALSE" ~ .quiet) ~ {is_true(.) || is_false(.)}
)

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
