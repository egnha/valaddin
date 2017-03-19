#' Reuse check formulae
#'
#' \code{localize} makes it easy to reuse check formulae: it converts a check
#' formula of global scope into a function that \emph{generates} corresponding
#' check formulae of local scope. \code{globalize} takes such a check-formula
#' generator and returns the underlying check formula (of global scope). These
#' operations are mutually invertible.
#'
#' @seealso The notion of \dQuote{scope} is explained in the \emph{Check
#'   Formulae} section of \link{firmly}.
#'
#'   Ready-made checkers for \link[=type-checkers]{types},
#'   \link[=bare-type-checkers]{bare types}, \link[=scalar-type-checkers]{scalar
#'   types}, and \link[=misc-checkers]{miscellaneous predicates} are provided as
#'   a convenience for the user, and as a model for creating families of check
#'   makers.
#' @examples
#' chk_pos_gbl <- "Not positive" ~ {. > 0}
#' chk_pos_lcl <- localize(chk_pos_gbl)
#' chk_pos_lcl(~x, "y not greater than x" ~ x - y)
#' # list("Not positive: x" ~ x, "y not greater than x" ~ x - y) ~ {. > 0}
#'
#' \dontrun{
#'
#' pass <- function(x, y) "Pass"
#'
#' # Impose local positivity checks
#' f <- firmly(pass, chk_pos_lcl(~x, "y not greater than x" ~ x - y))
#' f(2, 1)  # "Pass"
#' f(2, 2)  # Error: "y not greater than x"
#' f(0, 1)  # Errors: "Not positive: x", "y not greater than x"
#'
#' # Or just check positivity of x
#' g <- firmly(pass, chk_pos_lcl(~x))
#' g(1, 0)  # "Pass"
#' g(0, 0)  # Error: "Not positive: x"
#'
#' # In contrast, chk_pos_gbl checks positivity for all arguments
#' h <- firmly(pass, chk_pos_gbl)
#' h(2, 2)  # "Pass"
#' h(1, 0)  # Error: "Not positive: `y`"
#' h(0, 0)  # Errors: "Not positive: `x`", "Not positive: `y`"
#'
#' # Alternatively, globalize the localized checker
#' h2 <- firmly(pass, globalize(chk_pos_lcl))
#' all.equal(h, h2)  # TRUE
#' }
#'
#' # localize() and globalize() are mutual inverses
#' identical(globalize(localize(chk_pos_gbl)), chk_pos_gbl)  # TRUE
#' all.equal(localize(globalize(chk_pos_lcl)), chk_pos_lcl)  # TRUE
#'
#' @name scope-changing
NULL

is_check_maker <- function(x) {
  purrr::is_function(x) && inherits(x, "check_maker")
}

localize_ <- function(chk) {
  .msg <- ff_eval_lhs(chk)
  .rhs <- lazyeval::f_rhs(chk)
  .env <- lazyeval::f_env(chk)

  chkr <- function(...) {
    fs <- list(...)

    not_check_expr <- vapply(fs, Negate(is_check_expr), logical(1))
    if (any(not_check_expr)) {
      args <- paste(
        vapply(fs[not_check_expr], deparse_collapse, character(1)),
        collapse = ", "
      )
      stop_wo_call("LHS of formula(e) not empty or string: ", args)
    }

    wo_msg <- vapply(fs, is_onesided, logical(1))
    fs[wo_msg] <- lapply(fs[wo_msg], function(f) {
      ff_lhs(f) <- paste(.msg, deparse_collapse(lazyeval::f_rhs(f)), sep = ": ")
      f
    })

    # Cannot use lazyeval::f_new (<= 0.2.0) because lhs is not a language object
    ff_new(.rhs, fs, .env)
  }

  structure(chkr, class = c("check_maker", class(chkr)))
}

#' @rdname scope-changing
#' @export
#' @param chk Check formula of global scope \emph{with} a custom error message,
#'   i.e., a formula of the form \code{<string> ~ <predicate>}.
#' @return \code{localize} returns a function of class \code{"check_maker"} of
#'   call signature \code{function(...)}: the dots are formulae that are either
#'   one-sided or have a string LHS (a custom error message), which are
#'   interpreted as expressions to check, and the return value is the
#'   corresponding check formula of local scope, based on the predicate function
#'   of the global-scope check formula \code{chk}.
localize <- firmly(
  localize_,
  list("`chk` must be a formula of the form <string> ~ <predicate>" ~ chk) ~
    is_gbl_check_formula
)

globalize_ <- function(chkr) environment(chkr)$chk

#' @rdname scope-changing
#' @export
#' @param chkr Function of class \code{"check_maker"}, i.e., a function created
#'   by \code{localize}.
#' @return \code{globalize} returns the global-scope check formula underlying
#'   the function \code{chkr}.
globalize <- firmly(
  globalize_,
  list("`chkr` must be a local checker function (see ?localize)" ~ chkr) ~
    is_check_maker
)

#' @export
print.check_maker <- function(x, ...) {
  env <- environment(x)
  p <- env$.rhs

  cat("<check_maker>\n")

  cat("\n* Predicate function:\n")
  if (is_lambda(p)) {
    cat(deparse_collapse(express_lambda(p)), "\n")
  } else {
    print(p)
  }

  cat("\n* Error message:\n")
  cat(encodeString(env$.msg, quote = "\""), "\n")
}
