#' @include utils.R
NULL

#' Is a checklist valid?
#'
#' A valid checklist is a list of formulae \code{q ~ p}, where \code{p} is a
#' function (or a one-sided formulaic shorthand thereof, see
#' \code{\link[purrr]{as_function}()}), and \code{q} is a "check item" for
#' \code{p}; a check item is either empty, a string, or a list of formulae of
#' the form \code{~<expr>} or \code{<string> ~ <expr>}. (\code{p} is to be
#' called on \code{<expr>}.)
#'
#' @details \code{is_checklist()} is used to check the validity of the checks
#'   passed to \code{\link{strictly}()}, either via \code{...} or the argument
#'   \code{.checklist}.
#' @param x R object.
#' @return \code{TRUE} or \code{FALSE}, according to whether \code{x} is a valid
#'   checklist.
#' @export
#' @name checklist
is_checklist <- function(x) {
  if (is.list(x)) all(purrr::map_lgl(x, is_check_formula)) else FALSE
}

#' @rdname checklist
#' @export
is_check_formula <- function(x) {
  purrr::is_formula(x) && is_rhs_function(x) && is_lhs_checkitem(x)
}

is_rhs_function <- function(x) {
  rhs <- lazyeval::f_eval_rhs(x)
  is.function(rhs) || (purrr::is_formula(rhs) && length(rhs) == 2L)
}

is_lhs_checkitem <- function(x) {
  lhs <- lazyeval::f_eval_lhs(x)
  is.null(lhs) || purrr::is_scalar_character(lhs) || is_flist(lhs)
}

is_flist <- function(x) {
  is.list(x) &&
    all(purrr::map_lgl(x, function(.) {
      lhs <- lazyeval::f_eval_lhs(.)
      purrr::is_formula(.) && (is.null(lhs) || purrr::is_scalar_character(lhs))
    }))
}
