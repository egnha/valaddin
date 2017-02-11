#' @include functions.R
NULL

#' Is a formula a check formula?
#'
#' \code{is_check_formula(x)} checks whether \code{x} is a check formula, while
#' \code{is_checklist(x)} checks whether \code{x} is a \emph{checklist}, i.e., a
#' list of check formulae. Neither function verifies logical consistency.
#'
#' @param x R object.
#' @return \code{is_check_formula(x)}, resp. \code{is_checklist(x)}, returns
#'   \code{TRUE} or \code{FALSE}, according to whether \code{x} is or is not a
#'   check formula, resp. checklist.
#' @seealso \link{strictly}, for information on the specificaton and use of
#'   check formulae.
#' @examples
#' # Valid checklist
#' is_checklist(list(list(~x, ~y) ~ is.numeric, "Not positive" ~ {. > 0}))
#'
#' # Invalid checklists
#' is_checklist(list(is.numeric ~ list(~ x)))        # Backwards
#' is_checklist(list(list(log ~ x) ~ is.character))  # Invalid check item
#'
#' @name checklist
NULL

#' @rdname checklist
#' @export
#' @details If \code{is_check_formula()} cannot be evaluated on one of the
#'   components of a list \code{x}, then \code{is_checklist(x)} returns
#'   \code{FALSE}.
is_checklist <- function(x) {
  tryCatch(is_checklist_(x), error = function(e) FALSE)
}

is_checklist_ <- function(x) {
  if (is.list(x)) all(vapply(x, is_check_formula, logical(1))) else FALSE
}

#' @rdname checklist
#' @export
is_check_formula <- function(x) {
  purrr::is_formula(x) && is_rhs_function(x) && is_lhs_checkitem(x)
}

is_gbl_check_formula <- function(x) {
  purrr::is_formula(x) &&
    is_rhs_function(x) &&
    purrr::is_scalar_character(lazyeval::f_eval_lhs(x))
}

is_rhs_function <- function(x) {
  is_lambda(lazyeval::f_rhs(x)) || is.function(lazyeval::f_eval_rhs(x))
}

# Like magrittr, capture '{...}' as anonymous function
is_lambda <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("{"))
}

# To check that a formula is onesided, it is not enough to check
# is.null(lazyeval::f_eval_lhs(x)), for both NULL ~ x and ~x have NULL lhs.
is_onesided <- function(x) {
  length(x) == 2L
}

is_lhs_checkitem <- function(x) {
  lhs <- lazyeval::f_eval_lhs(x)
  is_onesided(x) || purrr::is_scalar_character(lhs) || is_flist(lhs)
}

is_flist <- function(x) {
  is.list(x) &&
    length(x) &&
    all(vapply(x, function(.) {
      purrr::is_formula(.) && {
        lhs <- lazyeval::f_eval_lhs(.)
        is_onesided(.) || purrr::is_scalar_character(lhs)
      }
    }, logical(1)))
}
