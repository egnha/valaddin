#' Decompose a strictly applied function
#'
#' Helper functions are provided to extract the components of a strictly applied
#' function:
#' \itemize{
#'   \item \code{strict_core()}: extracts the underlying \dQuote{nonstrict}
#'     function
#'   \item \code{strict_checks()}: extracts the checks
#'   \item \code{strict_args()}: extracts the names of arguments whose presence
#'     is to be checked
#' }
#' \code{is_strict_closure()} is a predicate function that checks whether an
#' object is a strictly applied function with checks or missing-argument
#' warning, i.e., a function of class \code{"strict_closure"}.
#'
#' @param x R object.
#' @return If \code{x} is a strictly applied function: \code{strict_core}
#'   returns a function; \code{strict_checks} returns a data frame with columns
#'   \code{expr} (language), \code{env} (environment), \code{string}
#'   (character), \code{msg} (character); \code{strict_args} returns a character
#'   vector. (In the absence of the component to be extracted, these functions
#'   return \code{NULL}.)
#' @examples
#' f <- function(x, y, ...) NULL
#' f_stc <- strictly(f, ~ is.numeric, list(~x, ~ y - x) ~ {. > 0})
#'
#' identical(strict_core(f_stc), f)                    # TRUE
#' strict_checks(f_stc)                                # 4 x 4 data frame
#' strict_args(f_stc)                                  # NULL
#' strict_args(strictly(f_stc, .warn_missing = TRUE))  # "x" "y"
#'
#' @name components
NULL

#' @rdname components
#' @export
is_strict_closure <- function(x) {
  purrr::is_function(x) && inherits(x, "strict_closure")
}

#' @rdname components
#' @export
strict_core <- function(x) {
  environment(environment(x)$.fn)$.f
}

#' @rdname components
#' @export
strict_checks <- function(x) {
  environment(x)$.chks
}

#' @rdname components
#' @export
strict_args <- function(x) {
  environment(environment(x)$.warn)$.ref_args
}