#' Decompose a firmly applied function
#'
#' Helper functions are provided to decompose a firmly applied function:
#' \itemize{
#'   \item \code{firm_core()}: extracts the underlying \dQuote{unfirm} function
#'   \item \code{firm_checks()}: extracts the checks
#'   \item \code{firm_args()}: extracts the names of arguments whose presence is
#'     to be checked
#' }
#'
#' @param x Object to decompose.
#' @return If \code{x} is a firmly applied function: \code{firm_core} returns a
#'   function; \code{firm_checks} returns a data frame with columns \code{expr}
#'   (language), \code{env} (environment), \code{string} (character), \code{msg}
#'   (character); \code{firm_args} returns a character vector. (In the absence
#'   of the component to be extracted, these functions return \code{NULL}.)
#' @seealso \code{\link{firmly}}
#' @examples
#' f <- function(x, y, ...) NULL
#' f_expl <- firmly(f, ~ is.numeric, list(~x, ~ y - x) ~ {. > 0})
#'
#' identical(firm_core(f_expl), f)                  # TRUE
#' firm_checks(f_expl)                              # 4 x 4 data frame
#' firm_args(f_expl)                                # NULL
#' firm_args(firmly(f_expl, .warn_missing = TRUE))  # "x" "y"
#'
#' @name components
NULL

#' @rdname components
#' @export
firm_core <- function(x) {
  environment(environment(x)$.fn)$.f
}

#' @rdname components
#' @export
firm_checks <- function(x) {
  environment(x)$.chks
}

#' @rdname components
#' @export
firm_args <- function(x) {
  environment(environment(x)$.warn)$.ref_args
}
