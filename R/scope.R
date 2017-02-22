#' @include strictly.R
NULL

#' Convert the scope of a check formula
#'
#' \code{localize()} converts a check formula of global scope into a
#' function that generates a check formulae of local scope;
#' \code{globalize()} takes such a function and returns the underlying
#' check formula (of global scope). These operations are mutually invertible.
#'
#' @seealso \link{strictly} explains the notion of "scope" for check formulae.
#' @examples
#' \dontrun{
#'
#' is_positive <- localize("Not positive" ~ {. > 0})
#' is_positive(x, x - y)
#' #> list("Not positive: `x`" ~ x, "Not positive: `x - y`" ~ x - y) ~ {. > 0}
#'
#' globalize(is_positive)
#' #> "Not positive" ~ {. > 0}
#' }
#'
#' @name scope
NULL

localize_ <- function(chk) {
  .msg <- lazyeval::f_eval_lhs(chk)
  .rhs <- lazyeval::f_rhs(chk)
  .env <- lazyeval::f_env(chk)

  chkr <- function(...) {
    args <- eval(substitute(alist(...)))
    parent <- parent.frame()
    lhs <- lapply(args, function(arg) {
      errmsg <- paste(.msg, deparse_collapse(arg), sep = ": ")
      f_new(arg, errmsg, parent)
    })

    f_new(.rhs, lhs, .env)
  }

  structure(chkr, class = c("check_maker", class(chkr)))
}

is_check_maker <- function(x) {
  purrr::is_function(x) && inherits(x, "check_maker")
}

globalize_ <- function(chkr) environment(chkr)$chk

#' @rdname scope
#' @export
#' @param chk Check formula of global scope with a custom error message, i.e., a
#'   formula of the form \code{<string> ~ <predicate>}, cf. \link{strictly}.
localize <- strictly(
  localize_,
  list("`chk` must be a formula of the form <string> ~ <predicate>" ~ chk) ~
    is_gbl_check_formula,
  .warn_missing = TRUE
)

#' @rdname scope
#' @export
#' @param chkr Function of class \code{"check_maker"}, i.e., created by
#'   \code{localize()}.
globalize <- strictly(
  globalize_,
  list("`chkr` must be a local checker function, see ?localize" ~ chkr) ~
    is_check_maker,
  .warn_missing = TRUE
)

#' @export
print.check_maker <- function(x, ...) {
  env <- environment(x)
  p <- env$.rhs

  cat("<check_maker>\n")

  cat("\n* Predicate function:\n")
  if (is_lambda(p))
    cat(deparse_collapse(expr_lambda(p)), "\n")
  else
    print(p)

  cat("\n* Error message:\n")
  cat(encodeString(env$.msg, quote = "\""), "\n")
}
