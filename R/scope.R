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
#'   \link[=bare-type-checkers]{bare types}, \link[=scalar-type-checkers]{scalar
#'   types}, and \link[=misc-checkers]{miscellaneous predicates} are provided as
#'   a convenience, and as a model for creating families of check makers.
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
#'   if (n <= 1) return(1)
#'   Recall(n - 1) + Recall(n - 2)
#' }
#' capped_fib <- firmly(fib, chk_lte(30, ~ ceiling(n)))
#' capped_fib(19)  # [1] 6765
#' capped_fib(31)  # Error: "Not <= 30: ceiling(n)"
#' }
#'
#' @name input-validators
NULL

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
localize <- function(...) {
  preds <- rlang::quos(...)
  if (length(preds) > 1) {
    warning("Only the first predicate will be localized", call. = FALSE)
  }
  pred <- get_predicate(preds[[1]], rlang::get_env(preds[[1]]))
  if (nzchar(names(preds)[1])) {
    msg <- names(preds)[1]
    protect <- FALSE
  } else {
    msg <- default_message(rlang::quo_expr(preds[[1]]), pred[["expr"]])
    protect <- TRUE
  }
  pred_quo <- rlang::new_quosure(pred[["fn"]], pred[["env"]])
  structure(
    function(...) {
      check_items <- rlang::quos(...)
      structure(
        rlang::new_formula(pred_quo, check_items, parent.frame()),
        def_err_msg = msg,
        protect_msg = protect
      )
    },
    class = c("local_predicate", "function")
  )
}

default_message <- function(expr1, expr2) {
  expr <- if (is_lambda(expr1)) expr2 else expr1
  # double-up braces so that generate_message() substitutes literal expression
  message_false(deparse_collapse(rlang::expr(UQ(expr)({{.}}))))
}

is_local_predicate <- function(x) {
  rlang::is_closure(x) && inherits(x, "local_predicate")
}

#' @rdname input-validators
#' @export
#' @param msg Error message (string).
#' @param compare Comparison function.
localize_comparison <- vld(
  "'msg' must be a string" = rlang::is_string ~ msg,
  "'compare' must be a function" = is.function ~ compare
)(function(msg, compare, ...) {
  force(msg)
  force(compare)
  env <- parent.frame()
  function(REF) {
    msg <- relevel_braces(glue_text(msg, env, list(REF = REF)))
    localize(UQ(msg) := function(x) compare(x, REF, ...))
  }
})

#' @rdname input-validators
#' @export
#' @param chkr Function of class `local_predicate`, i.e., a function created by
#'   `localize`.
#' @return `globalize` returns the global-scope check formula from which the
#'   function `chkr` is derived.
globalize <- vld(
  "'chkr' must be a local-check maker (function, see ?localize)" =
    is_local_predicate ~ chkr
)(function(chkr) {
  structure(
    environment(chkr)[["pred"]][["fn"]],
    def_err_msg = environment(chkr)[["msg"]],
    protect_msg = environment(chkr)[["protect"]]
  )
})

#' @export
print.local_predicate <- function(x, ...) {
  env <- environment(x)

  cat("<local_predicate>\n")

  cat("\n* Predicate function:\n")
  cat(deparse_collapse(env[["pred"]][["expr"]]), "\n")

  cat("\n* Error message:\n")
  cat(encodeString(env[["msg"]], quote = "\""), "\n")

  invisible(x)
}
