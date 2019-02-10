#' Generate input-validation checks
#'
#' \code{localize} derives a function that \emph{generates} check formulae of
#' local scope from a check formula of global scope. \code{globalize} takes such
#' a check-formula generator and returns the underlying global check formula.
#' These operations are mutually invertible.
#'
#' @seealso The notion of \dQuote{scope} is explained in the \emph{Check
#'   Formulae} section of \link{firmly}.
#'
#'   Ready-made checkers for \link[=type-checkers]{types},
#'   \link[=scalar-checkers]{scalar objects}, and
#'   \link[=misc-checkers]{miscellaneous predicates} are provided as a
#'   convenience, and as a model for creating families of check makers.
#' @examples
#' chk_pos_gbl <- "Not positive" ~ {. > 0}
#' chk_pos_lcl <- localize(chk_pos_gbl)
#' chk_pos_lcl(~x, "y not greater than x" ~ x - y)
#' # list("Not positive: x" ~ x, "y not greater than x" ~ x - y) ~ {. > 0}
#'
#' # localize and globalize are mutual inverses
#' identical(globalize(localize(chk_pos_gbl)), chk_pos_gbl)  # [1] TRUE
#' all.equal(localize(globalize(chk_pos_lcl)), chk_pos_lcl)  # [1] TRUE
#'
#' \dontrun{
#'
#' pass <- function(x, y) "Pass"
#'
#' # Impose local positivity checks
#' f <- firmly(pass, chk_pos_lcl(~x, "y not greater than x" ~ x - y))
#' f(2, 1)  # [1] "Pass"
#' f(2, 2)  # Error: "y not greater than x"
#' f(0, 1)  # Errors: "Not positive: x", "y not greater than x"
#'
#' # Or just check positivity of x
#' g <- firmly(pass, chk_pos_lcl(~x))
#' g(1, 0)  # [1] "Pass"
#' g(0, 0)  # Error: "Not positive: x"
#'
#' # In contrast, chk_pos_gbl checks positivity for all arguments
#' h <- firmly(pass, chk_pos_gbl)
#' h(2, 2)  # [1] "Pass"
#' h(1, 0)  # Error: "Not positive: `y`"
#' h(0, 0)  # Errors: "Not positive: `x`", "Not positive: `y`"
#'
#' # Alternatively, globalize the localized checker
#' h2 <- firmly(pass, globalize(chk_pos_lcl))
#' all.equal(h, h2)  # [1] TRUE
#'
#' # Use localize to make parameterized checkers
#' chk_lte <- function(n, ...) {
#'   err_msg <- paste("Not <=", as.character(n))
#'   localize(err_msg ~ {. <= n})(...)
#' }
#' fib <- function(n) {
#'   if (n <= 1L) return(1L)
#'   Recall(n - 1) + Recall(n - 2)
#' }
#' capped_fib <- firmly(fib, chk_lte(30, ~ ceiling(n)))
#' capped_fib(19)  # [1] 6765
#' capped_fib(31)  # Error: "Not <= 30: ceiling(n)"
#' }
#'
#' @name input-validators
NULL

is_check_maker <- function(x) {
  is_closure(x) && inherits(x, "check_maker")
}

#' @rdname input-validators
#' @export
#' @param chk Check formula of global scope \emph{with} custom error message,
#'   i.e., a formula of the form \code{<string> ~ <predicate>}.
#' @return \code{localize} returns a function of class \code{"check_maker"} and
#'   call signature \code{function(...)}:
#'   \itemize{
#'     \item The \code{\dots} are \strong{check items} (see \emph{Check Formulae
#'       of Local Scope} in the documentation page \link{firmly}).
#'     \item The return value is the check formula of local scope whose scope is
#'       comprised of these check items, and whose predicate function is that of
#'       \code{chk} (i.e., the right-hand side of \code{chk}). Unless a check
#'       item has its own error message, the error message is derived from that
#'       of \code{chk} (i.e., the left-hand side of \code{chk}).
#'   }
localize <- list(
  list("`chk` must be a formula of the form <string> ~ <predicate>" ~ chk) ~
    is_gbl_check_formula
) %checkin%
  function(chk) {
    .msg <- f_eval_lhs(chk)
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
        lazyeval::f_lhs(f) <- paste(.msg, deparse_collapse(lazyeval::f_rhs(f)), sep = ": ")
        f
      })

      lazyeval::f_new(.rhs, fs, .env)
    }

    structure(chkr, class = c("check_maker", class(chkr)))
  }

#' @rdname input-validators
#' @export
#' @param chkr Function of class \code{"check_maker"}, i.e., a function created
#'   by \code{localize}.
#' @return \code{globalize} returns the global-scope check formula from which
#'   the function \code{chkr} is derived.
globalize <- list(
  list("`chkr` must be a local checker function (see ?localize)" ~ chkr) ~
    is_check_maker
) %checkin%
  function(chkr) {
    environment(chkr)$chk
  }

#' @export
print.check_maker <- function(x, ...) {
  env <- environment(x)
  p <- env$.rhs

  cat("<check_maker>\n")

  cat("\n* Predicate function:\n")
  if (is_lambda(p)) {
    cat(deparse_collapse(express_lambda(p)), "\n")
  } else {
    print.default(p)
  }

  cat("\n* Error message:\n")
  cat(encodeString(env$.msg, quote = "\""), "\n")
}
