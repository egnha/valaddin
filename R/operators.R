chk_error_class <- vld(
  "'error_class' must be NULL or a character vector without NAs" :=
    {is.null(.) || is.character(.) && !anyNA(.)} ~ error_class
)
fasten_ <- function(..., error_class = NULL) {
  chk_parts <- parse_checks(vld(...))
  error_class <- error_class[nzchar(error_class)]
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    error_class_na <- is.null(firm_checks(f)) || length(error_class) == 0
    if (length(arg$nm) == 0 || is.null(chk_parts) && error_class_na) {
      return(f)
    }
    chks <-
      do.call("rbind", c(
        list(firm_checks(f)),
        list(chk_parts$local),
        list(localize_at(chk_parts$global, arg$sym))
      ))
    chks <- chks[rev(!duplicated(rev(chks$call))), , drop = FALSE]
    error_class <- error_class %||% firm_error(f) %||% "inputValidationError"
    as_firm_closure(
      with_sig(
        validation_closure(loosely(f), chks, sig, arg, error_class),
        sig,
        attributes(f)
      )
    )
  }
}
nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, sym = lapply(nm, as.symbol))
}
#' @export
loosely <- function(f) {
  if (!is.function(f))
    stop("'f' must be a function", call. = FALSE)
  else if (is_firm(f))
    .subset2(environment(f), "f")
  else
    f
}
#' @export
is_firm <- function(x) {
  inherits(x, "firm_closure")
}
as_firm_closure <- function(f) {
  if (!is_firm(f))
    class(f) <- c("firm_closure", class(f))
  f
}
with_sig <- function(f, sig, attrs) {
  formals(f) <- sig
  attributes(f) <- attrs
  f
}

#' @export
fasten <- fasten_(UQS(chk_error_class))(fasten_)

#' @export
firmly <- fasten(
  "'f' must be a function" := is.function ~ f,
  UQS(chk_error_class)
)(
  function(f, ..., error_class = NULL) {
    if (is.primitive(f))
      f <- rlang::as_closure(f)
    fasten_(..., error_class = error_class)(f)
  }
)

#' @export
print.firm_closure <- function(x, ...) {
  cat("<firm_closure>\n")
  cat("\n* Core function:\n")
  print.default(firm_core(x))
  cat("\n* Checks (<predicate>:<error message>):\n")
  chks <- firm_checks(x)
  if (length(chks)) {
    labels <- paste0(chks$call, ":\n", encodeString(chks$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }
  cat("\n* Error subclass for check errors:\n")
  subclass <- firm_error(x)
  if (!is.null(subclass))
    cat(paste(subclass, collapse = ", "), "\n")
  else
    cat("None\n")
  invisible(x)
}

#' @export
validate <- fasten(
  UQS(chk_error_class)
)(
  function(., ..., error_class = NULL) {
    validate <-
      loosely(validify)(..., error_class = error_class)
    eval(bquote(validate(.(substitute(.)))))
  }
)

#' @export
validify <- fasten(
  UQS(chk_error_class)
)(
  function(..., error_class = NULL) {
    error_class <- error_class %||% "objectValidationError"
    structure(
      fasten_(..., error_class = error_class)(pass),
      class = c("validator", "firm_closure", "function")
    )
  }
)
# name of argument must coincide with name of validate()'s object-argument
pass <- function(.) invisible(.)

#' @export
print.validator <- function(x, ...) {
  cat("<validator>\n")
  cat("\n* Validation (<predicate>:<error message>):\n")
  chks <- firm_checks(x)
  if (length(chks)) {
    labels <- paste0(chks$call, ":\n", encodeString(chks$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }
  cat("\n* Error subclass for validation errors:\n")
  subclass <- firm_error(x)
  if (!is.null(subclass))
    cat(paste(subclass, collapse = ", "), "\n")
  else
    cat("None\n")
  invisible(x)
}

# Documentation -----------------------------------------------------------

#' Apply a function firmly
#'
#' The main functions of valaddin apply or undo input validation checks to
#' functions:
#'   - `firmly()` transforms a function into a function with input validation
#'      checks.
#'   - `loosely()` undoes the application of `firmly()`, by returning the
#'      original function (without checks).
#'   - `fasten()` is a [currying](https://en.wikipedia.org/wiki/Currying) of
#'     `firmly()`: given a set of input validations, it returns a _functional
#'     operator_ that applies the input validations.
#'   - `is_firm()` is a predicate function that checks whether an object is a
#'     firmly applied function, i.e., a function created by `firmly().`
#'
#' @aliases firmly fasten loosely is_firm
#' @evalRd rd_usage(c("firmly", "fasten", "loosely", "is_firm"))
#'
#' @param f Function.
#' @param \dots Input validation checks.
#' @param error_class Subclass of the error condition to be raised when an input
#'   validation error occurs (character). If `NULL` (the default), the error
#'   subclass is `inputValidationError`.
#' @param x Object to test.
#'
#' @seealso [components], [validate].
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

#' Validate objects
#'
#' @aliases validate validify
#' @evalRd rd_usage(c("validate", "validify"))
#'
#' @param . Object to validate.
#' @param ... Input validation checks.
#' @param error_class Character vector of the error subclass to signal if
#'   validation fails. If `NULL` (the default), the error subclass is
#'   `objectValidationError`.
#'
#' @examples
#' # All assertions valid: data frame returned (invisibly)
#' validate(mtcars,
#'          is.data.frame,
#'          vld_all_map(is.numeric),
#'          vld_gt(10)(nrow(.)),
#'          vld_has_names(c("mpg", "cyl"))(.))
#'
#' \dontrun{
#' # Some assertions invalid: diagnostic error raised
#' validate(mtcars,
#'          is.matrix,
#'          vld_all_map(is.numeric),
#'          vld_gt(1000)(nrow(.)),
#'          vld_has_name("cylinders")(.))}
#'
#' @name validate
NULL