#' Decompose a firmly applied function
#'
#' Decompose a firmly applied function (i.e., a function created by
#' \code{\link{firmly}}):
#' \itemize{
#'   \item \code{firm_core} extracts the underlying \dQuote{core}
#'     function—the function that is called when all arguments are valid.
#'   \item \code{firm_checks} extracts the checks.
#'   \item \code{firm_error} extracts the subclass of the error condition that
#'     is signaled when an input validation error occurs.
#' }
#'
#' @param x Object to decompose.
#' @return If \code{x} is a firmly applied function:
#'   \itemize{
#'     \item \code{firm_core} returns a function.
#'     \item \code{firm_checks} returns a data frame with components \code{pred}
#'       (quosure), \code{expr} (quosure), \code{call} (character), \code{msg}
#'       (character), \code{is_msg_gbl} (logical), \code{env_msg} (environment).
#'     \item \code{firm_error} returns a character vector.
#'   }
#'   In the absence of the component to be extracted, these functions return
#'   \code{NULL}.
#'
#' @seealso \code{\link{firmly}}
#' @examples
#' f <- function(x, y, ...) NULL
#' ff <- firmly(f, is.numeric, {. > 0} ~ quos(x, y - x))
#' identical(firm_core(ff), f)  # [1] TRUE
#' firm_checks(ff)              # 4 x 6 data frame
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
firm_checks <- firm_closure_extractor("chks")

#' @rdname components
#' @export
firm_error <- firm_closure_extractor("error_class")
