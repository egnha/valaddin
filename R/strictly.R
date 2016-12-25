#' @include checks.R
NULL

#' Method for \code{lazyeval::find_data}
#'
#' @keywords internal
#' @export
find_data.environment <- function(x) x

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

# f: list("message" ~ arg, ...) ~ function
#' @export
normalize_flist <- function(f) {
  args <- do.call(lazyeval::f_list, lazyeval::f_eval_lhs(f))
  sym_pred <- lazyeval::f_rhs(f)
  is_empty <- names(args) == ""
  msg_auto <- purrr::map_chr(args[is_empty], function(.) {
    .call <- paste(
      trimws(deparse(
        substitute(p(arg), list(p = sym_pred, arg = lazyeval::f_rhs(.)))
      ), which = "both"),
      collapse = ""
    )
    paste(.call, "is FALSE")
  })
  names(args)[is_empty] <- msg_auto
  predicate <- purrr::as_function(lazyeval::f_eval_rhs(f))

  f_norm <- args ~ predicate
  eval(eval(substitute(substitute(x, lazyeval::f_env(f_norm)), list(x = f_norm))))
}

#' @export
eval_flist <- function(f, env) {
  args <- lazyeval::f_eval_lhs(f)
  pred <- lazyeval::f_eval_rhs(f)
  is_ok <- purrr::map_lgl(args, function(.) {
    pred(lazyeval::f_eval_rhs(., data = env))
  })
  paste(names(args)[!is_ok], collapse = "; ")
}

make_checklist <- function(x) {
  purrr::map_if(x, is_flist_chk, normalize_flist)
}

check_missing <- function(rarg) {
  substitute({
    `_args` <- names(match.call(expand.dots = FALSE)[-1L])
    `_msg` <- paste(setdiff(..rarg.., `_args`), collapse = ", ")
    if (`_msg` != "") {
      stop("Missing required arguments: ", `_msg`, call. = FALSE)
    }
  }, list(..rarg.. = rarg))
}

check_args <- function(chks, cond) {
  substitute({
    `_msgs` <- purrr::map_chr(..chks.., valaddin::eval_flist,
                              env = environment())
    `_is_not_empty` <- `_msgs` != ""
    if (any(`_is_not_empty`)) {
      `_msg` <- paste(`_msgs`[`_is_not_empty`], collapse = "; ")
      stop(..cond..(`_msg`), call. = FALSE)
    }
  }, list(..chks.. = chks, ..cond.. = cond))
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

#' @export
strictly_ <- function(.f, ..., .checklist = list(), .check_missing = FALSE,
                      .condition = NULL) {
  cond <- .condition %||% identity
  chks_orig <- c(list(...), .checklist)
  chks <- make_checklist(chks_orig)

  body_orig <- body(.f)
  sig <- formals(.f)
  rarg <- args_wo_defval(sig)
  chk_missing_args <- if (.check_missing) check_missing(rarg) else quote(NULL)
  chk_args <- if (length(chks)) check_args(chks, cond) else quote(NULL)
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
  attr(f, "..chks..") <- chks_orig
  attr(f, "..cond..") <- cond
  attr(f, "..rarg..") <- if (.check_missing) rarg else character(0)

  strict_closure(f)
}

chk_strictly <- list(
  list("`.f` not a (interpreted) function" ~ .f) ~ purrr::is_function,
  list("`.check_missing` not logical" ~ .check_missing) ~
    purrr::is_scalar_logical,
  list("`.condition` not `NULL` or a condition" ~ .condition) ~
    function(.) is.null(.) || is_condition(.),
  list("`.checklist` not a list of formula" ~ .checklist) ~ is_flist
  # "Checklist not formulae or list thereof" ~ is_valid_checklist(...),
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
strictly <- strictly_(
  strictly_,
  .checklist = chk_strictly, .check_missing = TRUE
)

#' @export
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
nonstrictly <- strictly_(
  nonstrictly_,
  list("`..f` not a closure" ~ ..f) ~ purrr::is_function, .check_missing = TRUE
)

get_strict_prop <- function(prop) {
  force(prop)
  chk <- list("`..f` not a strict closure" ~ ..f) ~ is_strict_closure
  strictly_(function(..f) attr(..f, prop, exact = TRUE), chk)
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
  x_ns <- nonstrictly_(x)
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
