is_predicate <- function(x) {
  purrr::is_function(x) ||
    (purrr::is_formula(x) && is_lambda(lazyeval::f_rhs(x)))
}

chks_mc <- list(
  list("`p` must be a function or lambda expression" ~ p) ~ is_predicate,
  list("`msg` must be a string" ~ msg) ~ purrr::is_scalar_character
)
make_check_ <- function(p, msg) {
  force(msg)
  if (purrr::is_formula(p))
    p <- lambda(lazyeval::f_rhs(p), parent.frame())

  function(...) {
    parent <- parent.frame()
    args <- eval(substitute(alist(...)))
    lhs <- lapply(args, lazyeval::f_new, lhs = msg, env = parent)
    lazyeval::f_new(p, lhs, env = parent)
  }
}

#' Customize check functions
#'
#' @param p Predicate function or lambda expression.
#' @param msg String.
#' @return Formula
#' @export
#' @examples
#' is_number <- make_check(purrr::is_scalar_numeric, "Not a number")
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#' secant <- strictly(secant, is_number(x, dx))
make_check <- strictly_(make_check_, .checklist = chks_mc, .warn_missing = TRUE)

# make_assertthat_check()