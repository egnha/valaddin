#' Is a formula a check formula?
#'
#' \code{is_check_formula(x)} checks whether \code{x} is a check formula, while
#' \code{is_checklist(x)} checks whether \code{x} is a \emph{checklist}, i.e., a
#' list of check formulae. (Neither function verifies logical consistency of the
#' implied checks.)
#'
#' @param x Object to test.
#' @return \code{is_check_formula}, resp. \code{is_checklist}, returns
#'   \code{TRUE} or \code{FALSE}, according to whether \code{x} is or is not a
#'   check formula, resp. checklist.
#' @seealso \link{firmly} (on the specification and use of check formulae)
#' @examples
#' is_check_formula(list(~x, ~y) ~ is.numeric)  # [1] TRUE
#' is_check_formula("Not positive" ~ {. > 0})   # [1] TRUE
#'
#' is_checklist(list(list(~x, ~y) ~ is.numeric, "Not positive" ~ {. > 0}))
#' # [1] TRUE
#'
#' # Invalid checklists
#' is_checklist("Not positive" ~ {. > 0})            # [1] FALSE (not a list)
#' is_checklist(list(is.numeric ~ list(~ x)))        # [1] FALSE (backwards)
#' is_checklist(list(list(log ~ x) ~ is.character))  # [1] FALSE (invalid check item)
#'
#' @name checklist
NULL

#' @rdname checklist
#' @export
is_check_formula <- function(x) {
  inherits(x, "formula") && is_rhs_function(x) && is_lhs_checkitem(x)
}

#' @rdname checklist
#' @export
is_checklist <- function(x) {
  is.list(x) && all(vapply(x, is_check_formula, logical(1)))
}

is_string <- function(x) {
  typeof(x) == "character" && length(x) == 1L && !isTRUE(is.na(x))
}

is_gbl_check_formula <- function(x) {
  inherits(x, "formula") && is_rhs_function(x) && is_string(f_eval_lhs(x))
}

is_rhs_function <- function(x) {
  is_lambda(lazyeval::f_rhs(x)) || is.function(f_eval_rhs(x))
}

# Like magrittr, capture '{...}' as anonymous function
is_lambda <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("{"))
}

# To check that a formula is onesided, it is not enough to check
# is.null(f_eval_lhs(x)), for both NULL ~ x and ~x have NULL lhs.
is_onesided <- function(x) {
  length(x) == 2L
}

is_f_onesided <- function(x) {
  inherits(x, "formula") && is_onesided(x)
}

is_lhs_checkitem <- function(x) {
  is_onesided(x) || {
    lhs <- f_eval_lhs(x)
    is_string(lhs) || is_flist(lhs)
  }
}

is_check_expr <- function(x) {
  inherits(x, "formula") && (is_onesided(x) || is_string(f_eval_lhs(x)))
}

is_flist <- function(x) {
  is.list(x) && length(x) && all(vapply(x, is_check_expr, logical(1)))
}
