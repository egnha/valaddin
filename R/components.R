#' Decompose a firmly applied function
#'
#' Decompose a firmly applied function (i.e., a function created by [firmly()]
#' or [fasten()]):
#' - `firm_core()` extracts the underlying \dQuote{core} function—the function
#'   that is called when all arguments are valid
#' - `firm_error()` extracts the subclass of the error condition that is
#'   signaled when an input validation error occurs
#'
#' @details For primitive functions, `firm_core()` returns the underlying
#'   function as a closure.
#' @param x Object to decompose.
#' @return If `x` is a firmly applied function:
#'   - `firm_core()` returns a function
#'   - `firm_error()` returns a character vector
#'
#'   In the absence of the component to be extracted, these functions return
#'   `NULL` (in particular, whenever `x` is not of class `firm_closure`).
#'
#' @seealso [firmly()]
#' @examples
#' f <- function(x, y, ...) NULL
#' ff <- firmly(f, is.numeric, {isTRUE(. > 0)} ~ vld(x, y - x))
#' identical(firm_core(ff), f)  # [1] TRUE
#' firm_error(ff)               # [1] "inputValidationError"
#'
#' @name components
NULL

firm_closure_extractor <- function(this) {
  force(this)
  function(x)
    if (is_firm(x)) .subset2(environment(x), this) else NULL
}

#' @rdname components
#' @export
firm_core <- firm_closure_extractor("f")

#' @rdname components
#' @export
firm_error <- firm_closure_extractor("error_class")

firm_checks <- firm_closure_extractor("chks")
