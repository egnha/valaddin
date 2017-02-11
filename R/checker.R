#' @include strictly.R
NULL

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

  structure(chkr, class = c("local_checker", class(chkr)))
}

is_local_checker <- function(x) inherits(x, "local_checker")

globalize_check_ <- function(chkr) {
  env <- environment(chkr)
  lazyeval::f_new(env$rhs, env$msg, env$env)
}

#' Convert the scope of a check formula
#'
#' \code{localize_check()} converts a check formula of global scope into a
#' function that generates a check formulae of local scope;
#' \code{globalize_check()} takes such a function and returns the underlying
#' check formula (of global scope). These operations are mutually invertible.
#'
#' @seealso \link{strictly} explains the notion of "scope" for check formulae.
#' @examples
#' is_positive <- localize_check("Not positive" ~ {. > 0})
#' is_positive(x, x - y)
#' #> list("Not positive: `x`" ~ x, "Not positive: `x - y`" ~ x - y) ~ {. > 0}
#'
#' globalize_check(is_positive)
#' #> "Not positive" ~ {. > 0}
#'
#' @name scope
NULL

#' @rdname scope
#' @export
#' @param chk Check formula of global scope with a custom error message, i.e., a
#'   formula of the form \code{<string> ~ <predicate>}, cf. \link{strictly}.
localize_check <- strictly_(
  localize_check_,
  list("`chk` must be a formula of the form <string> ~ <predicate>" ~ chk) ~
    is_gbl_check_formula,
  .warn_missing = TRUE
)

#' @rdname scope
#' @export
#' @param chkr Function of class \code{"local_checker"}, i.e., a function
#'   created by \code{localize_check()}.
globalize_check <- strictly_(
  globalize_check_,
  list("`chkr` local checker function, see ?localize_check" ~ chkr) ~
    is_local_checker,
  .warn_missing = TRUE
)

#' @export
print.local_checker <- function(x, ...) {
  env <- environment(x)
  p <- env$rhs

  cat("<local_checker>\n")

  cat("\n* Predicate function:\n")
  p <- if (is_lambda(p)) expr_lambda(p) else p
  cat(deparse_collapse(p), "\n")

  cat("\n* Error message:\n")
  cat(encodeString(env$msg, quote = "\""), "\n")
}
