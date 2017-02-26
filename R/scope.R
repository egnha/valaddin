#' @include strictly.R
NULL

#' Make a reusable check formula
#'
#' \code{localize()} converts a check formula of global scope into a function
#' that \emph{generates} corresponding check formulae of local scope. This makes
#' it easy to specify reusable checks. \code{globalize()} takes such a
#' check-formula generator and returns the underlying check formula (of global
#' scope). \code{localize()} and \code{globalize()} are mutually invertible.
#'
#' @seealso Documentation for \code{\link{strictly}()} explains the notion of
#'   \dQuote{scope} in the context of check formulae.
#' @examples
#' \dontrun{
#'
#' chk_pos_gbl <- "Not positive" ~ {. > 0}
#' chk_pos_lcl <- localize(chk_pos_gbl)
#' chk_pos_lcl(~x, ~x - y)
#' # list("Not positive: x" ~ x, "Not positive: x - y" ~ x - y) ~ {. > 0}
#'
#' pass <- function(x, y) "Pass"
#'
#' # Impose local positivity checks
#' f <- strictly(pass, chk_pos_lcl(~x, ~x - y))
#' f(2, 1)  # "Pass"
#' f(2, 2)  # Error: "Not positive: x - y"
#' f(0, 1)  # Errors: "Not positive: x", "Not positive: x - y"
#'
#' # Or just check positivity of x
#' g <- strictly(pass, chk_pos_lcl(~x))
#' g(1, 0)  # "Pass"
#' g(0, 0)  # Error: "Not positive: x"
#'
#' # In contrast, chk_pos_gbl checks positivity for all arguments
#' h <- strictly(pass, chk_pos_gbl)
#' h(2, 2)  # "Pass"
#' h(1, 0)  # Error: "Not positive: y"
#' h(0, 0)  # Errors: "Not positive: x", "Not positive: y"
#'
#' # Alternatively, globalize the localized checker
#' h2 <- strictly(pass, globalize(chk_pos_lcl))
#' all.equal(h, h2)  # TRUE
#'
#' # localize() and globalize() are mutual inverses
#' identical(globalize(localize(chk_pos_gbl)), chk_pos_gbl)  # TRUE
#' all.equal(localize(globalize(chk_pos_lcl)), chk_pos_lcl)  # TRUE
#' }
#'
#' @name scope
NULL

is_check_maker <- function(x) {
  purrr::is_function(x) && inherits(x, "check_maker")
}

localize_ <- function(chk) {
  .msg <- lazyeval::f_eval_lhs(chk)
  .rhs <- lazyeval::f_rhs(chk)
  .env <- lazyeval::f_env(chk)

  chkr <- function(...) {
    fs <- list(...)

    not_f_onesided <- vapply(fs, Negate(is_f_onesided), logical(1))
    if (any(not_f_onesided)) {
      args <- paste(
        vapply(fs[not_f_onesided], deparse_collapse, character(1)),
        collapse = ", "
      )
      stop("Not one-sided formula(e) (see ?localize): ", args, call. = FALSE)
    }

    parent <- parent.frame()
    lhs <- lapply(fs, function(f) {
      errmsg <- paste(.msg, deparse_collapse(lazyeval::f_rhs(f)), sep = ": ")
      lazyeval::f_lhs(f) <- errmsg
      f
    })

    f_new(.rhs, lhs, .env)
  }

  structure(chkr, class = c("check_maker", class(chkr)))
}

#' @rdname scope
#' @export
#' @param chk Check formula of global scope with a custom error message, i.e., a
#'   formula of the form \code{<string> ~ <predicate>}, cf.
#'   \code{\link{strictly}()}.
#' @return \code{localize()} returns a function of class \code{"check_maker"}
#'   with signature \code{function(...)}, which expects one-sided formulae as
#'   arguments.
localize <- strictly(
  localize_,
  list("`chk` must be a formula of the form <string> ~ <predicate>" ~ chk) ~
    is_gbl_check_formula,
  .warn_missing = TRUE
)

globalize_ <- function(chkr) environment(chkr)$chk

#' @rdname scope
#' @export
#' @param chkr Function of class \code{"check_maker"}, i.e., created by
#'   \code{localize()}.
#' @return \code{globalize()} returns the underlying global check formula.
globalize <- strictly(
  globalize_,
  list("`chkr` must be a local checker function (see ?localize)" ~ chkr) ~
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
