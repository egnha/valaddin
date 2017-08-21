chk_error_class <- vld(
  "'error_class' must be NULL or a character vector without NAs" :=
    {is.null(.) || is.character(.) && !anyNA(.)} ~ error_class
)
fasten_ <- function(..., error_class = NULL) {
  checks <- parse_checks(vld(...))
  assemble_checks <- function(checks_prev, args) {
    check_all_args <- check_at_args(args)
    chks <- do.call("rbind", c(
      list(checks_prev),
      list(checks$local),
      list(check_all_args(checks$global))
    ))
    chks <- chks[rev(!duplicated(rev(chks$call))), , drop = FALSE]
    chks
  }
  error_class <- error_class[nzchar(error_class)]
  function(f) {
    sig <- formals(f)
    args <- nomen(sig)
    error_class_na <- is_empty(firm_checks(f)) || is_empty(error_class)
    if (is_empty(args) || is_empty(checks) && error_class_na)
      return(f)
    chks <- assemble_checks(firm_checks(f), args)
    error_class <- error_class %||% firm_error(f) %||% "inputValidationError"
    fasten_checks(f, chks, sig, args, error_class)
  }
}
deduplicate <- function(xs, by) {
  xs[rev(!duplicated(rev(xs[[by]]))), , drop = FALSE]
}
fasten_checks <- function(f, chks, sig, args, error_class) {
  fn_fastened <- validation_closure(loosely(f), chks, sig, args, error_class)
  as_firm_closure(with_sig(fn_fastened, sig, attributes(f)))
}
#' @export
loosely <- function(f) {
  if (!is.function(f))
    abort("'f' must be a function")
  else if (is_firm(f))
    .subset2(environment(f), "f")
  else
    f
}
#' @export
is_firm <- check_is_class("firm_closure")
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
      f <- as_closure(f)
    fasten_(..., error_class = error_class)(f)
  }
)

#' @export
validate <- fasten(
  UQS(chk_error_class)
)(
  function(., ..., error_class = NULL) {
    validate <- loosely(validify)(..., error_class = error_class)
    eval_bare(bquote(validate(.(substitute(.)))))
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
#' @section Specifying input validations: _TODO_ (see the examples)
#'
#' @seealso [components], [validate].
#' @examples
#' \dontrun{
#'
#' bc <- function(x, y) c(x, y, 1 - x - y)
#'
#' ## Ensure that inputs are numeric
#' bc1 <- firmly(bc, is.numeric)
#'
#' bc1(.5, .2)
#' #> [1] 0.5 0.2 0.3
#'
#' bc1(.5, ".2")
#' #> Error: bc1(x = 0.5, y = ".2")
#' #> FALSE: is.numeric(y)
#'
#' ## Use custom error messages
#' bc2 <- firmly(bc, "{{.}} is not numeric (type: {typeof(.)})" := is.numeric)
#'
#' bc2(.5i, ".2")
#' #> Error: bc2(x = 0+0.5i, y = ".2")
#' #> 1) x is not numeric (type: complex)
#' #> 2) y is not numeric (type: character)
#'
#' ## firmly() and fasten() support tidyverse idioms
#' z <- 0
#' in_triangle <- vld(
#'   "{{.}} is not positive (value is {.})" :=
#'   {isTRUE(. > !! z)} ~ vld(x, y, 1 - x - y)
#' )
#' bc3 <- firmly(bc, is.numeric, !!! in_triangle)
#'
#' bc3(.5, .2)
#' #> [1] 0.5 0.2 0.3
#'
#' bc3(.5, .6)
#' #> Error: bc3(x = 0.5, y = 0.6)
#' #> 1 - x - y is not positive (value is -0.1)
#'
#' ## Use fasten() to highlight the core logic
#' bc_clean <- fasten(
#'   "{{.}} is not a number" := {is.numeric(.) && length(.) == 1},
#'   "{{.}} is not positive" :=
#'   {isTRUE(. > 0)} ~
#'     vld(x, "y is not in the upper-half plane" := y, 1 - x - y)
#' )(
#'   function(x, y) {
#'     c(x, y, 1 - x - y)
#'   }
#' )
#'
#' ## Recover the underlying function with loosely()
#' print(loosely(bc_clean))
#' #> function(x, y) {
#' #>     c(x, y, 1 - x - y)
#' #>   }
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
#'          vld_gt(10, nrow(.)),
#'          vld_has_names(c("mpg", "cyl")))
#'
#' # Some assertions invalid: diagnostic error raised
#' \dontrun{
#' validate(mtcars,
#'          is.matrix,
#'          vld_all_map(is.numeric),
#'          vld_gt(1000, nrow(.)),
#'          vld_has_name("cylinders"))}
#'
#' @name validate
NULL