#' @include strictly.R
NULL

is_unary <- function(x) {
  is.function(x) && length(nomen(formals(x))$wo_value) == 1L
}
is_rhs_lambda <- function(x) {
  purrr::is_formula(x) && is_onesided(x) && is_lambda(lazyeval::f_rhs(x))
}
is_predicate <- function(x) {
  is_unary(x) || is_rhs_lambda(x)
}

chk_mc <- list(
  list("`p` must be a function or lambda expression (see ?make_check)" ~ p) ~
    is_predicate,
  list("`msg` must be a string" ~ msg) ~ purrr::is_scalar_character
)
make_check_ <- function(msg, p) {
  force(msg)
  p_symb <- if (purrr::is_formula(p)) lazyeval::f_rhs(p) else substitute(p)

  function(...) {
    args <- eval(substitute(alist(...)))
    nms <- vapply(args, deparse_collapse, character(1))
    parent <- parent.frame()
    lhs <- unfurl_args(msg, nms, args, parent)

    lazyeval::f_new(p_symb, lhs, parent)
  }
}

#' Customize check functions
#'
#' @param p Predicate function or lambda expression.
#' @param msg String.
#' @return Formula
#' @export
#' @examples
#' \dontrun{
#' is_number <- make_check(purrr::is_scalar_numeric, "Not a number")
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#' secant <- strictly(secant, is_number(x, dx))
#' }
make_check <- strictly_(make_check_, .checklist = chk_mc, .warn_missing = TRUE)

# make_assertthat_check()