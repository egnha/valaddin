#' @include strictly.R
NULL

is_gbl_check_formula <- function(x) {
  is_check_formula(x) && purrr::is_scalar_character(lazyeval::f_eval_lhs(x))
}

localize_check_ <- function(chk) {
  msg <- lazyeval::f_eval_lhs(chk)
  rhs <- lazyeval::f_rhs(chk)
  env <- lazyeval::f_env(chk)

  chkr <- function(...) {
    args <- eval(substitute(alist(...)))
    parent <- parent.frame()
    lhs <- lapply(args, function(arg) {
      nm <- deparse_collapse(arg)
      err <- paste(msg, encodeString(nm, quote = "`"), sep = ": ")
      lazyeval::f_new(arg, err, parent)
    })

    lazyeval::f_new(rhs, lhs, env)
  }

  structure(chkr, class = c("strict_checker", class(chkr)))
}

#' Localize a global check formula
#'
#' @param chk Global check formula.
#' @return Local check formula.
#' @examples
#' \dontrun{
#' is_number <- localize_check("Not a number" ~ purrr::is_scalar_numeric)
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#' secant <- strictly(secant, is_number(x, dx))
#' }
#' @export
#' @name localize_check
localize_check <- strictly_(
  localize_check_,
  list("`chk` must be a formula of the form <string> ~ <predicate>" ~ chk) ~
    is_gbl_check_formula,
  .warn_missing = TRUE
)

#' @export
print.strict_checker <- function(x, ...) {
  env <- environment(x)
  p <- env$rhs

  cat("<strict_checker>\n")

  cat("\n* Predicate function:\n")
  p <- if (is_lambda(p)) expr_lambda(p) else p
  cat(deparse_collapse(p), "\n")

  cat("\n* Error message:\n")
  cat(encodeString(env$msg, quote = "\""), "\n")
}

# make_assertthat_check()