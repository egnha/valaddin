#' @include checks.R utils.R
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
#' # `foo` and `foo_strict` have the same parent environment
#' identical(parent.env(environment(foo)), parent.env(environment(foo_strict)))
#' # `environment(foo_strict)` has the bindings of `environment(foo)`, at the
#' # point where `foo_strict` is defined
#' e <- setdiff(names(environment(foo)), "foo_strict")
#' identical(sort(names(environment(foo_strict))), sort(e))
#'
#' foo_stricter <- strictly(foo_strict,
#'                          "Not numeric" ~ is.numeric(x) && is.numeric(y))
#' foo_stricter(1, 2)                     # "sum: 3"
#' foo_stricter(1, 2, "SUM:", bogus_obj)  # "SUM: 3"
#' foo_stricter(1, "2")                   # Error: Not numeric
#' foo_stricter(1, 2, NA_real_)           # "NA 3"
#'
#' foo_strictest <- strictly(foo_stricter,
#'                           "Not string" ~ purrr::is_scalar_character(a),
#'                           .chk_missing = TRUE)
#' foo_strictest(1, 2)            # "sum: 3"
#' foo_strictest(1, "2")          # Error: Not numeric
#' foo_strictest(1, 2, NA_real_)  # Error: Not string
#' foo_strictest(1, a = "foo")    # Error: Missing required arguments: y
#'
#' bar <- function(x, y) x - y
#' is_positive <- function(x) x >= 0
#' bar_strict <- strictly(bar, ~is.numeric, ~is_positive)
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

#' Create a failure message for a value, predicate pair
#'
#' @param x R object.
#' @param p Predicate function.
#' @return String.
#' @keywords internal
f_errmsg <- function(x, p) {
  .x <- lazyeval::expr_find(x)
  .p <- lazyeval::expr_find(p)
  if (p(x)) {
    character(0)
  } else {
    # FIX: Might have length greater than 1!
    paste(deparse(substitute(p(x), list(p = .p, x = .x))), "is FALSE")
  }
}

f_message <- function(f) {
  msg <- lazyeval::f_rhs(f)
  p <- lazyeval::f_lhs(f)
  p ~ function(.) if (.) character(0) else msg
}

#' Elaborate a one-sided formula
#'
#' As an argument validator, a one-sided formula \code{~p} is a shorthand for
#' submitting each and every (explicit) argument of a function to a common
#' check. In order to implement this shorthand, a one-side formula must be
#' elaborated as a two-side formula that can perform the check on all arguments,
#' and produce an appropriate failure message. \code{f_onesided()} performs this
#' elaboration.
#'
#' @param f One-sided formula.
#' @param args Promises.
#' @param sep Separator (string).
#' @return Two-sided formula.
#' @keywords internal
f_onesided <- function(f, l_args, sep = "; ") {
  p <- lazyeval::f_eval_rhs(f)
  q <- function(xs) {
    paste(purrr::map_chr(xs, f_errmsg, p = p), collapse = sep)
  }
  q ~ l_args  # should these be lazy objects?
}

# eval(lazyeval::call_new(lazyeval::f_eval_rhs(f), lazyeval::f_eval_lhs(f)),
#      lazyeval::f_env(f), `_env`)

f_string <- function(f) {
  string <- lazyeval::f_lhs(f)
  p <- lazyeval::f_rhs(f)
  q <- function(x) {
    if (x) character(0) else string
  }
  q ~ p
}

normalize <- function(x, n = -2L) {
  l <- unpack(x)
  l
}

check_missing <- function(rarg, sep = ", ") {
  substitute({
    "_args" <- names(match.call()[-1L])
    "_args_missing" <- setdiff(..rarg.., `_args`)
    if (length(`_args_missing`) > 0L) {
      "_msg" <- paste(`_args_missing`, collapse = ..sep..)
      stop("Missing required arguments: ", `_msg`, call. = FALSE)
    }
  }, list(..rarg.. = rarg, ..sep.. = sep))
}
check_args <- function(chks, cond, sep = "; ") {
  substitute({
    "_env" <- as.list(environment(), all.names = TRUE)
    "_pass" <- purrr::map_lgl(..chks.., lazyeval::f_eval_rhs, data = `_env`)
    if (!all(`_pass`)) {
      "_msg" <- paste(
        purrr::map_chr(..chks..[!`_pass`], lazyeval::f_eval_lhs, data = `_env`),
        collapse = ..sep..
      )
      stop(..cond..(`_msg`))
    }
  }, list(..chks.. = chks, ..cond.. = cond, ..sep.. = sep))
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
  attrs <- c("..body..", "..chks..", "..cond..", "..rarg..")
  purrr::is_function(x) &&
    inherits(x, "strict_closure") &&
    all(attrs %in% names(attributes(x)))
}

strictly_ <- function(.f, ..., .cond = NULL, .chk_missing = FALSE) {
  cond <- .cond %||% identity
  # Provisional: `unpack` should be replaced by `normalize` (evtl.)
  chks <- unpack(list(...))

  body_orig <- body(.f)
  sig <- formals(.f)
  rarg <- args_wo_defval(sig)
  chk_missing_args <- if (.chk_missing) check_missing(rarg) else quote(NULL)
  chk_args <- if (length(chks) == 0L) quote(NULL) else check_args(chks, cond)
  body <- substitute({
    ..chk_missing_args..
    ..chk_args..
    ..body..
  }, list(..body..             = body_orig,
          ..chk_missing_args.. = chk_missing_args,
          ..chk_args..         = chk_args))

  f <- eval(call("function", sig, as.call(body)))
  environment(f) <- clone_env(environment(.f))
  attributes(f)  <- attributes(.f)
  attr(f, "..body..") <- body_orig
  attr(f, "..chks..") <- chks
  attr(f, "..cond..") <- cond
  attr(f, "..rarg..") <- if (.chk_missing) rarg else character(0)

  strict_closure(f)
}

chk_strictly <- list(
  "Not an (interpreted) function: `.f`" ~ purrr::is_function(.f),
  "Checklist not formulae or list thereof" ~ is_valid_checklist(...),
  "Must be `NULL` or a condition: `.cond`" ~ is_valid_cond(.cond),
  "Not logical: `.chk_missing`" ~ purrr::is_scalar_logical(.chk_missing)
)

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
strictly <- strictly_(strictly_, chk_strictly, .chk_missing = TRUE)

nonstrictly_ <- function(..f) {
  ns_class <- class(..f)[class(..f) != "strict_closure"]
  if (!is_strict_closure(..f)) {
    class(..f) <- ns_class
    ..f
  } else {
    body <- attr(..f, "..body..", exact = TRUE)
    f <- eval(call("function", formals(..f), body))
    environment(f) <- clone_env(environment(..f))
    attrs <- c("..body..", "..chks..", "..cond..", "..rarg..")
    attributes(f) <- attributes(..f)[setdiff(names(attributes(..f)), attrs)]
    class(f) <- ns_class
    f
  }
}

#' @rdname strictly
#' @param ..f Strict function, i.e., function of class \code{"strict_closure"}.
#' @export
nonstrictly <- strictly(nonstrictly_,
                        "Not a closure: `..f`" ~ purrr::is_function(..f),
                        .chk_missing = TRUE)

chk_strict_closure <- "Not a strict closure: `..f`" ~ is_strict_closure(..f)

get_strict_prop <- function(prop, chk = chk_strict_closure) {
  force(prop)
  force(chk)
  strictly(function(..f) {
    attr(..f, prop, exact = TRUE)
  }, chk)
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
strict_body <- get_strict_prop("..body..")

#' @rdname strictly
#' @export
strict_check <- get_strict_prop("..chks..")

#' @rdname strictly
#' @export
strict_condition <- get_strict_prop("..cond..")

#' @rdname strictly
#' @export
strict_reqarg <- get_strict_prop("..rarg..")

#' @export
print.strict_closure <- function(x) {
  chks <- strict_check(x)
  cond <- strict_condition(x)
  rarg <- strict_reqarg(x)
  x_ns <- nonstrictly(x)
  environment(x_ns) <- environment(x)

  cat("<strict closure>\n")
  cat("\n* Body:\n")
  print(x_ns)
  cat("\n* Check missing arguments:\n")
  cat(if (length(rarg) == 0L) "<none>" else rarg, "\n")
  cat("\n* Checks:\n")
  if (length(chks) == 0L) cat("<none>\n") else print_enumerate(chks)
  cat("\n* Condition:\n")
  if (identical(cond, identity)) cat("<default error handler>") else print(cond)
}
