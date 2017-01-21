#' Is a checklist correctly formed?
#'
#' \code{is_checklist(x)} checks whether \code{x} is a \emph{checklist}, i.e., a
#' list of argument-validation checks. It does not verify logical consistency of
#' the checks.
#'
#' A checklist is a list of \emph{check
#' formulae} \code{q ~ p}, where
#' \itemize{
#'   \item \code{p} is a predicate function of one argument, which may be
#'     specified anonymously as a function body between \code{\{...\}} with
#'     \code{.} (or \code{.x}) as the argument (cf.
#'     \code{purrr::\link[purrr]{as_function}()}, and the example, below),
#'   \item \code{q} is a \emph{check item} for \code{p}.
#' }
#' A check item is either empty, a string (failure message), or a list of
#' formulae of the form \code{~<expr>} (auto-generated failure message) or
#' \code{<string> ~ <expr>} (custom failure message). The predicate \code{p}
#' performs its check by being called on \code{<expr>}.
#'
#' \code{is_checklist()} is used to check the validity of the checks passed to
#' \code{\link{strictly}()}, either via \code{...} or the argument
#' \code{.checklist}.
#'
#' @seealso \link{strictly}, for more information on the specificaton and use of
#'   check formulae.
#'
#' @param x R object.
#' @return \code{TRUE} or \code{FALSE}, according to whether \code{x} is a valid
#'   checklist.
#' @export
#' @examples
#' # Valid checklist
#' is_checklist(list(list(~x, ~y) ~ is.numeric, "Not positive" ~ {. > 0}))
#'
#' # Invalid checklists
#' is_checklist(list(is.numeric ~ list(~ x)))        # Backwards
#' is_checklist(list(list(log ~ x) ~ is.character))  # Invalid check item
#' @name checklist
is_checklist <- function(x) {
  tryCatch(is_checklist_(x), error = function(e) FALSE)
}

is_checklist_ <- function(x) {
  if (is.list(x)) all(purrr::map_lgl(x, is_check_formula)) else FALSE
}

#' @rdname checklist
#' @export
is_check_formula <- function(x) {
  purrr::is_formula(x) && is_rhs_function(x) && is_lhs_checkitem(x)
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
    all(purrr::map_lgl(x, function(.) {
      purrr::is_formula(.) && {
        lhs <- lazyeval::f_eval_lhs(.)
        is_onesided(.) || purrr::is_scalar_character(lhs)
      }
    }))
}
