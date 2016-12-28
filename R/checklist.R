#' @include utils.R
NULL

#' Is a checklist correctly formed?
#'
#' \code{is_checklist(x)} checks whether \code{x} is a list of
#' argument-validation checks, i.e., a \emph{checklist}. A checklist is a list
#' of \emph{check formulae} \code{q ~ p}, where
#' \itemize{
#'   \item \code{p} is a predicate function, or one-sided formulaic shorthand
#'     thereof (see \code{purrr::\link[purrr]{as_function}()}),
#'   \item \code{q} is a \emph{check item} for \code{p}.
#' }
#' A check item is either empty, a string, or a list of formulae of the form
#' \code{~<expr>} or \code{<string> ~ <expr>}. (The predicate \code{p} performs
#' its check by being called on \code{<expr>}.)
#'
#' @details \code{is_checklist()} is used to check the validity of the checks
#'   passed to \code{\link{strictly}()}, either via \code{...} or the argument
#'   \code{.checklist}.
#' @param x R object.
#' @return \code{TRUE} or \code{FALSE}, according to whether \code{x} is a valid
#'   checklist.
#' @export
#' @examples
#' # Valid checklist
#' is_checklist(list(list(~x, ~y) ~ is.numeric, "Not positive" ~ ~{. > 0}))
#'
#' # Invalid checklists
#' is_checklist(list(is.numeric ~ list(~ x)))
#' is_checklist(list(list(log ~ x) ~ is.character))
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
