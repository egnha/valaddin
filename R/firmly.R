# Checking infrastructure -------------------------------------------------

# Make a list of check items
unfurl <- function(.symb, .nm, .msg, .env) {
  chk_items <- lapply(.symb, lazyeval::f_new, env = .env)
  if (is.null(.msg)) {
    names(chk_items) <- character(length(chk_items))
  } else {
    names(chk_items) <- paste(.msg, encodeString(.nm, quote = "`"), sep = ": ")
  }

  chk_items
}

checks_df <- function(pred, items, env, string) {
  n <- length(items)
  x <- list(
    expr   = lapply(items, function(.) as.call(c(pred, lazyeval::f_rhs(.)))),
    env    = `[<-`(vector("list", n), list(env)),
    string = string,
    msg    = names(items)
  )
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}

# Assemble a data frame of checks from a check formula
assemble <- function(.chk, .nm, .symb, .env = lazyeval::f_env(.chk)) {
  p <- lazyeval::f_rhs(.chk)
  p_expr <- if (is_lambda(p)) express_lambda(p) else p
  predicate <- eval(p_expr, .env)

  lhs <- f_eval_lhs(.chk)
  chk_items <- if (is.list(lhs)) {
    # .chk: local scope
    do.call(lazyeval::f_list, lhs)
  } else {
    # .chk: global scope (lhs: string/NULL)
    unfurl(.symb, .nm, lhs, .env)
  }
  string <- vapply(chk_items, deparse_call, character(1), fn_expr = p_expr)
  is_blank <- !nzchar(names(chk_items))
  names(chk_items)[is_blank] <- sprintf("FALSE: %s", string[is_blank])

  checks_df(predicate, chk_items, .env, string)
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
      warning_wo_call(msg)
    }

    invisible(.call)
  }
}

warning_closure <- function(.fn, .warn) {
  force(.fn)
  force(.warn)

  function() {
    call <- match.call()
    encl <- parent.env(environment())
    encl$.warn(call)
    eval.parent(`[[<-`(call, 1L, encl$.fn))
  }
}

# Validation apparatus ----------------------------------------------------

problems <- function(chks, verdict) {
  vapply(seq_along(verdict), function(i) {
    x <- verdict[[i]]
    if (is_false(x)) {
      chks$msg[[i]]
    } else if (inherits(x, "error")) {
      sprintf("Error evaluating check %s: %s", chks$string[[i]], x$message)
    } else {
      sprintf("Predicate value %s not TRUE/FALSE: %s",
              chks$string[[i]], deparse_collapse(x))
    }
  }, character(1))
}

#' @importFrom stats runif setNames
validating_closure <- function(.chks, .sig, .nm, .fn, .warn, .error_class) {
  force(.fn)
  force(.warn)
  force(.error_class)

  # Input-validation environment
  PROM.ENV <- sprintf("__PROM.ENV__%.12f", runif(1L))
  make_promises <- eval(call("function", .sig, quote(environment())))
  ve <- new.env(parent = emptyenv())
  promises <- function(call, env_call) {
    ve[[PROM.ENV]] <- eval(`[[<-`(call, 1L, make_promises), env_call)
    parent.env(ve[[PROM.ENV]]) <- environment(.fn)
    ve
  }

  # Ensure that promises in validation expressions are from ve[["PROM.ENV"]]
  subs <- lapply(setNames(nm = .nm), function(.)
    substitute(get(., e), list(. = ., e = as.name(PROM.ENV)))
  )
  exprs <- lapply(seq_len(nrow(.chks)), function(i) {
    expr <- .chks$expr[[i]]
    # 'expr' is a call, so its second component is the call arguments
    expr[[2L]] <- eval(substitute(substitute(., subs), list(. = expr[[2L]])))
    list(expr = expr, env = .chks$env[[i]])
  })

  deparse_w_defval <- function(call) {
    .sig[names(call[-1L])] <- call[-1L]
    .sig <- .sig[!vapply(.sig, identical, logical(1), quote(expr = ))]
    deparse_collapse(as.call(c(call[[1L]], .sig)))
  }
  error <- function(message) {
    structure(
      list(message = message, call = NULL),
      class = c(.error_class, "error", "condition")
    )
  }

  # Local bindings to avoid (unlikely) clashes with formal arguments
  enumerate_many <- match.fun("enumerate_many")
  problems <- match.fun("problems")

  function() {
    call <- match.call()
    encl <- parent.env(environment())
    encl$.warn(call)
    env <- encl$promises(call, parent.frame())
    verdict <- suppressWarnings(lapply(encl$exprs, function(.)
      tryCatch(eval(.$expr, `parent.env<-`(env, .$env)), error = identity)
    ))
    pass <- vapply(verdict, isTRUE, logical(1))

    if (all(pass)) {
      eval.parent(`[[<-`(call, 1L, encl$.fn))
    } else {
      fail <- !pass
      msg_call  <- encl$deparse_w_defval(call)
      msg_error <- encl$enumerate_many(
        encl$problems(encl$.chks[fail, ], verdict[fail])
      )
      stop(encl$error(paste(msg_call, msg_error, sep = "\n")))
    }
  }
}

# Functional operators ----------------------------------------------------

# Represent non-dot arguments by name and symbol
# sig: pairlist
nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, symb = lapply(nm, as.symbol))
}

skip <- function(...) invisible()

firmly_ <- function(.f, ..., .checklist = list(),
                    .warn_missing = character(), .error_class = character()) {
  chks <- unname(c(list(...), .checklist))

  error_class_inapplicable <- is.null(firm_checks(.f))
  if (!length(chks) && !length(.warn_missing) && error_class_inapplicable) {
    return(.f)
  }

  if (!is_checklist(chks)) {
    stop_wo_call("Invalid check formula(e)")
  }

  sig <- formals(.f)
  arg <- nomen(sig)
  if (!length(arg$nm)) {
    if (length(.warn_missing)) {
      stop_wo_call("Invalid `.warn_missing`: `.f` has no named argument")
    }
    # No arguments, so assume .error_class inapplicable, hence chks non-empty
    warning_wo_call("Check formula(e) not applied: `.f` has no named argument")
    return(.f)
  }

  arg_unknown <- !(.warn_missing %in% arg$nm)
  if (any(arg_unknown)) {
    stop_wo_call(sprintf("Invalid `.warn_missing`: %s not argument(s) of `.f`",
                         quote_collapse(.warn_missing[arg_unknown])))
  }

  fn <- if (is_firm(.f)) firm_core(.f) else .f
  pre_chks <- firm_checks(.f)
  maybe_warn <- warn(.warn_missing) %||% warn(firm_args(.f)) %||% skip
  error_class <- .error_class %||% firm_error(.f) %||% "simpleError"

  if (length(chks)) {
    asm_chks <- unique(
      do.call("rbind",
              c(list(pre_chks),
                lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb)))
    )
    f <- validating_closure(asm_chks, sig, arg$nm, fn, maybe_warn, error_class)
  } else {
    # .warn_missing or .error_class is non-empty
    f <- if (is.null(pre_chks)) {
      warning_closure(fn, maybe_warn)
    } else {
      validating_closure(pre_chks, sig, arg$nm, fn, maybe_warn, error_class)
    }
  }

  firm_closure(with_sig(f, sig, .attrs = attributes(.f)))
}

is_closure <- function(x) typeof(x) == "closure"
#' @export
is_firm <- function(x) {
  is_closure(x) && inherits(x, "firm_closure")
}

firm_closure <- function(.f) {
  .f <- match.fun(.f)
  if (!inherits(.f, "firm_closure")) {
    class(.f) <- c("firm_closure", class(.f))
  }
  .f
}

#' @export
firmly <- firmly_(
  firmly_,
  list("`.f` not an interpreted function" ~ .f) ~ is_closure,
  list("`.checklist` not a list" ~ .checklist) ~ is.list,
  list(
    "`.warn_missing` not a character vector" ~ .warn_missing,
    "`.error_class` not a character vector" ~ .error_class
  ) ~ {is.character(.) && !anyNA(.)}
)

#' @export
`%checkin%` <- function(.checks, .f) {
  nms <- names(.checks) %||% character(length(.checks))
  firmly(
    .f,
    .checklist    = .checks[!nms %in% c(".warn_missing", ".error_class")],
    .warn_missing = .checks[[".warn_missing"]] %||% character(),
    .error_class  = .checks[[".error_class"]] %||% character()
  )
}

#' @export
loosely <- function(.f, .keep_check = FALSE, .keep_warning = FALSE) {
  if (!inherits(.f, "firm_closure") || .keep_check && .keep_warning) {
    return(.f)
  }

  f_chks <- if (.keep_check) firm_checks(.f) else NULL
  f_args <- if (.keep_warning) firm_args(.f) else NULL

  if (is.null(f_chks) && is.null(f_args)) {
    return(firm_core(.f))
  }

  sig <- formals(.f)
  f <- if (is.null(f_chks)) {
    warning_closure(firm_core(.f), warn(f_args))
  } else {
    validating_closure(f_chks, sig, nomen(sig)$nm,
                       firm_core(.f), skip, firm_error(.f))
  }

  firm_closure(with_sig(f, sig, .attrs = attributes(.f)))
}

# Printing ----------------------------------------------------------------

#' @export
print.firm_closure <- function(x, ...) {
  cat("<firm_closure>\n")

  cat("\n* Core function:\n")
  print.default(firm_core(x))

  cat("\n* Checks (<predicate>:<error message>):\n")
  calls <- firm_checks(x)
  if (length(calls)) {
    labels <- paste0(calls$string, ":\n", encodeString(calls$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }

  cat("\n* Error subclass for check errors:\n")
  subclass <- firm_error(x)
  if (!is.null(subclass)) {
    cat(paste(subclass, collapse = ", "), "\n")
  } else {
    cat("None\n")
  }

  cat("\n* Check for missing arguments:\n")
  args <- firm_args(x)
  if (!is.null(args) && length(args)) {
    cat(quote_collapse(args), "\n")
  } else {
    cat("Not checked\n")
  }
}

# Documentation -----------------------------------------------------------

#' Apply a function firmly
#'
#' \code{firmly} transforms a function into a function with input validation
#' checks. \code{loosely} undoes the application of \code{firmly}, by returning
#' the original function (without checks). \code{is_firm} is a predicate
#' function that checks whether an object is a firmly applied function, i.e.,
#' a function created by \code{firmly}.
#' \cr\cr
#' Use \code{\%checkin\%} to apply \code{firmly} as an operator. Since this
#' allows you to keep checks and arguments adjacent, it is the preferred way to
#' use \code{firmly} in scripts and packages.
#'
#' @aliases firmly %checkin% loosely is_firm
#' @evalRd rd_usage(c("firmly", "%checkin%", "loosely", "is_firm"))
#'
#' @param .f Interpreted function, i.e., closure.
#' @param \dots Input-validation check formula(e).
#' @param .checklist List of check formulae. (These are combined with check
#'   formulae provided via \code{\dots}.)
#' @param .warn_missing Arguments of \code{.f} whose absence should raise a
#'   warning (character).
#' @param .error_class Subclass of the error condition to be raised when an
#'   input validation error occurs (character).
#' @param .checks List of check formulae, optionally containing character
#'   vectors named \code{.warn_missing}, \code{.error_class}, corresponding to
#'   the similarly named arguments.
#' @param .keep_check,.keep_warning Should existing checks, resp.
#'   missing-argument warnings, be kept?
#' @param x Object to test.
#'
#' @section Check Formulae:
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
#'   applied to: whether the condition is to be applied to all
#'   (non-\code{\dots}) arguments of \code{.f} (the case of \dQuote{global
#'   scope}), or whether the condition is to be selectively applied to certain
#'   expressions of the arguments (the case of \dQuote{local scope}).
#'
#'   According to \strong{scope}, there are two classes of check formulae:
#'   \itemize{
#'     \item \strong{Check formulae of global scope}
#'       \preformatted{<string> ~ <predicate>}
#'       \preformatted{~<predicate>}
#'
#'     \item \strong{Check formulae of local scope}
#'       \preformatted{list(<check_item>, <check_item>, ...) ~ <predicate>}
#'   }
#'
#'   \subsection{Check Formulae of Global Scope}{
#'     A \strong{global check formula} is a succinct way of asserting that the
#'     function \code{<predicate>} returns \code{TRUE} when called on each
#'     (non-\code{\dots}) argument of \code{.f}. Each argument for which
#'     \code{<predicate>} \emph{fails}—returns \code{FALSE} or is itself not
#'     evaluable—produces an error message, which is auto-generated unless a
#'     custom error message is supplied by specifying the string
#'     \code{<string>}.
#'
#'     \subsection{Example}{
#'       The condition that all (non-\code{\dots}) arguments of a function must
#'       be numerical can be enforced by the check formula
#'       \preformatted{~is.numeric}
#'       or
#'       \preformatted{"Not numeric" ~ is.numeric}
#'       if the custom error message \code{"Not numeric"} is to be used (in lieu
#'       of an auto-generated error message).
#'     }
#'   }
#'
#'   \subsection{Check Formulae of Local Scope}{
#'     A \strong{local check formula} imposes argument-specific conditions. Each
#'     \strong{check item} \code{<check_item>} is a formula of the form \code{~
#'     <expression>} (one-sided) or \code{<string> ~ <expression>}; it imposes
#'     the condition that the function \code{<predicate>} is \code{TRUE} for the
#'     expression \code{<expression>}. As for global check formulae, each check
#'     item for which \code{<predicate>} fails produces an error message, which
#'     is auto-generated unless a custom error message is supplied by a string
#'     as part of the left-hand side of the check item (formula).
#'
#'     \subsection{Example}{
#'       The condition that \code{x} and \code{y} must differ for the function
#'       \code{function(x, y) \{1 / (x - y)\}} can be enforced by the local
#'       check formula
#'       \preformatted{list(~x - y) ~ function(.) abs(.) > 0}
#'       or
#'       \preformatted{list("x, y must differ" ~ x - y) ~ function(.) abs(.) > 0}
#'       if the custom error message \code{"x, y must differ"} is to be used (in
#'       lieu of an auto-generated error message).
#'     }
#'   }
#'
#'   \subsection{Anonymous Predicate Functions}{
#'     Following the
#'     \href{https://cran.r-project.org/package=magrittr}{\pkg{magrittr}}
#'     package, an anonymous (predicate) function of a single argument \code{.}
#'     can be concisely expressed by enclosing the body of such a function
#'     within curly braces \code{\{ \}}.
#'
#'     \subsection{Example}{
#'       The (onsided, global) check formula
#'       \preformatted{~{. > 0}}
#'       is equivalent to the check formula \code{~function(.) {. > 0}}
#'     }
#'   }
#'
#' @section Value:
#'   \subsection{\code{firmly}}{
#'     \code{firmly} does nothing when there is nothing to do: \code{.f} is
#'     returned, unaltered, when both \code{.checklist} and \code{.warn_missing}
#'     are empty, or when \code{.f} has no named argument and
#'     \code{.warn_missing} is empty.
#'
#'     Otherwise, \code{firmly} again returns a function that behaves
#'     \emph{identically} to \code{.f}, but also performs input validation:
#'     before a call to \code{.f} is attempted, its inputs are checked, and if
#'     any check fails, an error halts further execution with a message
#'     tabulating every failing check. (If all checks pass, the call to
#'     \code{.f} respects lazy evaluation, as usual.)
#'
#'     \subsection{Subclass of the input-validation error object}{
#'       The subclass of the error object is \code{.error_class}, unless
#'       \code{.error_class} is \code{character()}. In the latter case, the
#'       subclass of the error object is that of the existing error object, if
#'       \code{.f} is itself a firmly applied function, or it is
#'       \code{"simpleError"}, otherwise.
#'     }
#'
#'     \subsection{Formal Arguments and Attributes}{
#'       \code{firmly} preserves the attributes and formal arguments of
#'       \code{.f} (except that the \code{"class"} attribute gains the component
#'       \code{"firm_closure"}, unless it already contains it).
#'     }
#'   }
#'   \subsection{\code{\%checkin\%}}{
#'     \code{\%checkin\%} applies the check formula(e) in the list \code{.checks}
#'     to \code{.f}, using \code{firmly}. The \code{.warn_missing} and
#'     \code{.error_class} arguments of \code{firmly} may be specified as named
#'     components of \code{.checks}.
#'   }
#'   \subsection{\code{loosely}}{
#'     \code{loosely} returns \code{.f}, unaltered, when \code{.f} is not a
#'     firmly applied function, or both \code{.keep_check} and
#'     \code{.keep_warning} are \code{TRUE}.
#'
#'     Otherwise, \code{loosely} returns the underlying (original) function,
#'     stripped of any input validation checks imposed by \code{firmly}, unless
#'     one of the flags \code{.keep_check}, \code{.keep_warning} is switched on:
#'     if \code{.keep_check}, resp. \code{.keep_warning}, is \code{TRUE},
#'     \code{loosely} retains any existing checks, resp. missing-argument
#'     warnings, of \code{.f}.
#'   }
#'   \subsection{\code{is_firm}}{
#'     \code{is_firm} returns \code{TRUE} if \code{x} is a firmly applied
#'     function (i.e., has class \code{"firm_closure"}), and \code{FALSE},
#'     otherwise.
#'   }
#'
#' @seealso \code{firmly} is enhanced by a number of helper functions:
#'   \itemize{
#'     \item To verify that a check formula is syntactically correct, use the
#'       predicates \code{\link{is_check_formula}}, \code{\link{is_checklist}}.
#'     \item To make custom check-formula generators, use
#'       \code{\link{localize}}.
#'     \item Pre-made check-formula generators are provided to facilitate
#'       argument checks for \link[=type-checkers]{types},
#'       \link[=scalar-checkers]{scalar objects}, and
#'       \link[=misc-checkers]{other} common data structures and input
#'       assumptions. These functions are prefixed by \code{vld_}, for
#'       convenient browsing and look-up in editors and IDE's that support name
#'       completion.
#'     \item To access the components of a firmly applied function, use
#'       \code{\link{firm_core}}, \code{\link{firm_checks}},
#'       \code{\link{firm_error}}, \code{\link{firm_args}}, (or simply
#'       \code{\link[base]{print}} the function to display its components).
#'   }
#'
#' @examples
#' \dontrun{
#'
#' dlog <- function(x, h) (log(x + h) - log(x)) / h
#'
#' # Require all arguments to be numeric (auto-generated error message)
#' dlog_fm <- firmly(dlog, ~is.numeric)
#' dlog_fm(1, .1)    # [1] 0.9531018
#' dlog_fm("1", .1)  # Error: "FALSE: is.numeric(x)"
#'
#' # Require all arguments to be numeric (custom error message)
#' dlog_fm <- firmly(dlog, "Not numeric" ~ is.numeric)
#' dlog_fm("1", .1)  # Error: "Not numeric: `x`"
#'
#' # Alternatively, "globalize" a localized checker (see ?localize, ?globalize)
#' dlog_fm <- firmly(dlog, globalize(vld_numeric))
#' dlog_fm("1", .1)  # Error: "Not double/integer: `x`"
#'
#'# Predicate functions can be specified anonymously or by name
#' dlog_fm <- firmly(dlog, list(~x, ~x + h, ~abs(h)) ~ function(x) x > 0)
#' dlog_fm <- firmly(dlog, list(~x, ~x + h, ~abs(h)) ~ {. > 0})
#' is_positive <- function(x) x > 0
#' dlog_fm <- firmly(dlog, list(~x, ~x + h, ~abs(h)) ~ is_positive)
#' dlog_fm(1, 0)  # Error: "FALSE: is_positive(abs(h))"
#'
#' # Describe checks individually using custom error messages
#' dlog_fm <-
#'   firmly(dlog,
#'          list("x not positive" ~ x, ~x + h, "Division by 0 (=h)" ~ abs(h)) ~
#'            is_positive)
#' dlog_fm(-1, 0)
#' # Errors: "x not positive", "FALSE: is_positive(x + h)", "Division by 0 (=h)"
#'
#' # Specify checks more succinctly by using a (localized) custom checker
#' req_positive <- localize("Not positive" ~ is_positive)
#' dlog_fm <- firmly(dlog, req_positive(~x, ~x + h, ~abs(h)))
#' dlog_fm(1, 0)  # Error: "Not positive: abs(h)"
#'
#' # Combine multiple checks
#' dlog_fm <- firmly(dlog,
#'                   "Not numeric" ~ is.numeric,
#'                   list(~x, ~x + h, "Division by 0" ~ abs(h)) ~ {. > 0})
#' dlog_fm("1", 0)  # Errors: "Not numeric: `x`", check-eval error, "Division by 0"
#'
#' # Any check can be expressed using isTRUE
#' err_msg <- "x, h differ in length"
#' dlog_fm <- firmly(dlog, list(err_msg ~ length(x) - length(h)) ~ {. == 0L})
#' dlog_fm(1:2, 0:2)  # Error: "x, h differ in length"
#' dlog_fm <- firmly(dlog, list(err_msg ~ length(x) == length(h)) ~ isTRUE)
#' dlog_fm(1:2, 0:2)  # Error: "x, h differ in length"
#'
#' # More succinctly, use vld_true
#' dlog_fm <- firmly(dlog, vld_true(~length(x) == length(h), ~all(abs(h) > 0)))
#' dlog_fm(1:2, 0:2)
#' # Errors: "Not TRUE: length(x) == length(h)", "Not TRUE: all(abs(h) > 0)"
#'
#' dlog_fm(1:2, 1:2)  # [1] 0.6931472 0.3465736
#'
#' # loosely recovers the underlying function
#' identical(loosely(dlog_fm), dlog)  # [1] TRUE
#'
#' # Use .warn_missing when you want to ensure an argument is explicitly given
#' # (see vignette("valaddin") for an elaboration of this particular example)
#' as_POSIXct <- firmly(as.POSIXct, .warn_missing = "tz")
#' Sys.setenv(TZ = "EST")
#' as_POSIXct("2017-01-01 03:14:16")  # [1] "2017-01-01 03:14:16 EST"
#'                                    # Warning: "Argument(s) expected ... `tz`"
#' as_POSIXct("2017-01-01 03:14:16", tz = "UTC")  # [1] "2017-01-01 03:14:16 UTC"
#' loosely(as_POSIXct)("2017-01-01 03:14:16")     # [1] "2017-01-01 03:14:16 EST"
#'
#' # Use firmly to constrain undesirable behavior, e.g., long-running computations
#' fib <- function(n) {
#'   if (n <= 1L) return(1L)
#'   Recall(n - 1) + Recall(n - 2)
#' }
#' fib <- firmly(fib, list("`n` capped at 30" ~ ceiling(n)) ~ {. <= 30L})
#' fib(21)  # [1] 17711 (NB: Validation done only once, not for every recursive call)
#' fib(31)  # Error: `n` capped at 30
#'
#' # Apply fib unrestricted
#' loosely(fib)(31)  # [1] 2178309 (may take several seconds to finish)
#'
#' # firmly won't force an argument that's not involved in checks
#' g <- firmly(function(x, y) "Pass", list(~x) ~ is.character)
#' g(c("a", "b"), stop("Not signaled"))  # [1] "Pass"
#'
#' # In scripts and packages, it is recommended to use the operator %checkin%
#' vec_add <- list(
#'   ~is.numeric,
#'   list(~length(x) == length(y)) ~ isTRUE,
#'   .error_class = "inputError"
#' ) %checkin%
#'   function(x, y) {
#'     x + y
#'   }
#'
#' # Or call firmly with .f explicitly assigned to the function
#' vec_add2 <- firmly(
#'   ~is.numeric,
#'   list(~length(x) == length(y)) ~ isTRUE,
#'   .f = function(x, y) {
#'     x + y
#'   },
#'   .error_class = "inputError"
#' )
#'
#' all.equal(vec_add, vec_add2)  # [1] TRUE
#' }
#'
#' @name firmly
NULL
