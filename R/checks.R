#' Is a checklist valid?
#'
#' A valid checklist is either a list of formulas or a list thereof. An empty
#' list is considered valid.
#'
#' @details \code{is_valid_checklist()} is used to check the validity of the
#'   checklist passed to \code{strictly()}.
#' @param ... R objects.
#' @return \code{TRUE} or \code{FALSE}, according to whether \code{list(...)} is
#'   a valid checklist.
#' @keywords internal
is_valid_checklist <- function(...) {
  is_fml <- function(x) purrr::map_lgl(x, purrr::is_formula)
  dots <- list(...)
  if (length(dots) == 0L) {
    TRUE
  } else {
    d1 <- dots[[1L]]
    is_fml_list <- length(dots) == 1L && is.list(d1) && all(is_fml(d1))
    is_fml_dots <- all(is_fml(dots))
    is_fml_list || is_fml_dots
  }
}

#' Is a condition valid?
#'
#' @details \code{is_valid_cond()} is used to check the validity of the
#'   condition argument (\code{.cond}) of \code{strictly()}.
#' @param x R object.
#' @return \code{TRUE} or \code{FALSE}, according to whether \code{x} is
#'   a valid value for \code{.cond} in \code{strictly()}.
#' @keywords internal
is_valid_cond <- function(x) {
  is.null(x) || is_condition(x)
}

is_fmessage <- function(x) {
  if (!purrr::is_formula(x)) {
    FALSE
  } else {
    purrr::is_scalar_character(lazyeval::f_lhs(x)) &&
      is.function(lazyeval::f_rhs(x))
  }
}

is_fonesided <- function(x) {
  purrr::is_formula(x) && is.null(lazyeval::f_lhs(x))
}

is_flist <- function(x) {
  is.list(x) && all(purrr::map_lgl(x, purrr::is_formula))
}
