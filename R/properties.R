#' Properties of a strict closure
#'
#' @param ..f Strict closure, i.e., interpreted function of class
#'   \code{"strict_closure"}.
#' @name properties
NULL

#' @rdname properties
#' @param x R object.
#' @export
is_strict_closure <- function(x) {
  purrr::is_function(x) && inherits(x, "strict_closure")
}

#' @rdname properties
#' @export
strict_core <- function(..f) {
  environment(environment(..f)$.fn)$.f
}

#' @rdname properties
#' @export
strict_checks <- function(..f) {
  environment(..f)$.chks
}

#' @rdname properties
#' @export
strict_args <- function(..f) {
  environment(environment(..f)$.warn)$.ref_args
}