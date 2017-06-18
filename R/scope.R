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
  dots <- rlang::quos(...)
  if (length(dots) > 1) {
    warning("Only the first argument will be localized", call. = FALSE)
  }
  p <- as_predicate(dots[[1]], rlang::get_env(dots[[1]]))
  localize_(names(dots[1]), p[["fn"]], p[["expr"]], rlang::quo_expr(dots[[1]]))
}

localize_ <- function(msg, fn, expr, expr_q = expr) {
  force(fn)
  if (nzchar(msg)) {
    protect <- FALSE
  } else {
    msg <- default_message(expr, expr_q)
    protect <- TRUE
  }
  structure(
    function(...) {
      check_items <- rlang::quos(...)
      structure(
        rlang::new_formula(fn, check_items, parent.frame()),
        def_err_msg = msg,
        protect_msg = protect
      )
    },
    class = c("local_predicate", "function")
  )
}

default_message <- function(expr1, expr2) {
  expr <- if (is_lambda(expr2)) expr1 else expr2
  # double-up braces so that generate_message() substitutes literal expression
  message_false(deparse_collapse(rlang::expr(UQ(expr)({{.}}))))
}

is_local_predicate <- function(x) {
  rlang::is_closure(x) && inherits(x, "local_predicate")
}

#' @export
print.local_predicate <- function(x, ...) {
  env <- environment(x)
  cat("<local_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(env[["expr"]]), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(env[["msg"]], quote = "\""), "\n")
  invisible(x)
}

#' @rdname input-validators
#' @export
localize_comparison <- function(...) {
  dots <- rlang::quos(...)
  if (length(dots) > 1) {
    warning("Only the first argument will be localized", call. = FALSE)
  }
  msg <- names(dots[1])
  env <- rlang::get_env(dots[[1]])
  cmp <- as_comparison(dots[[1]])
  function(.ref, ...) {
    repr <- list(expr = deparse_collapse(substitute(.ref)), value = .ref)
    msg <- glue_text(msg, env, list(.ref = repr), .open = "{{{", .close = "}}}")
    dot_exprs <- rlang::exprs(...)
    expr_bd <- eval(bquote(substitute(.(cmp[["expr_bd"]]), list(.ref = .ref))))
    expr <- function_expr(eval(bquote(rlang::expr(.(expr_bd)))))
    pred <- eval(
      function_expr(cmp[["expr_fn"]], alist(.ref = , ... = )), cmp[["env"]]
    )(.ref, ...)
    localize_(msg, pred, expr)
  }
}

as_comparison <- function(q) {
  expr <- body <- rlang::get_expr(q)
  if (is_lambda(expr)) {
    env <- rlang::get_env(q)
  } else if (is.function(x <- try_eval_tidy(q))) {
    env <- environment(x)
    body <- bquote(.(body)(., .ref, UQS(dot_exprs)))
    expr <- bquote(.(expr)(., .ref, ...))
  } else {
    stop(err_not_function(q, x), call. = FALSE)
  }
  list(expr_bd = body, expr_fn = function_expr(expr), env = env)
}

#' @rdname input-validators
#' @export
#' @param chkr Function of class `local_predicate`, i.e., a function created by
#'   `localize`.
#' @return `globalize` returns the global-scope check formula from which the
#'   function `chkr` is derived.
globalize <- vld(
  "'chkr' must be a local predicate (see ?localize)" =
    is_local_predicate ~ chkr
)(function(chkr) {
  env <- environment(chkr)
  structure(
    env[["fn"]],
    def_err_msg = env[["msg"]],
    protect_msg = env[["protect"]],
    expr        = env[["expr"]],
    class       = c("global_predicate", "function")
  )
})

#' @export
print.global_predicate <- function(x, ...) {
  cat("<global_predicate>\n")
  cat("\n* Predicate function:\n")
  cat(deparse_collapse(attr(x, "expr", exact = TRUE)), "\n")
  cat("\n* Error message:\n")
  cat(encodeString(attr(x, "def_err_msg", exact = TRUE), quote = "\""), "\n")
  invisible(x)
}

#' @export
predicate_function <- function(x) {
  UseMethod("predicate_function")
}
#' @export
predicate_function.local_predicate <- function(x) {
  environment(x)[["fn"]]
}
#' @export
predicate_function.global_predicate <- function(x) {
  strip_attr(x, "def_err_msg", "protect_msg", "expr", "class")
}
strip_attr <- function(x, ...) {
  attrs <- list(...)
  Reduce(function(., which) `attr<-`(., which, NULL), attrs, x)
}

#' @export
predicate_message <- function(x) {
  UseMethod("predicate_message")
}
#' @export
predicate_message.local_predicate <- function(x) {
  environment(x)[["msg"]]
}
#' @export
predicate_message.global_predicate <- function(x) {
  attr(x, "def_err_msg", exact = TRUE)
}

#' @export
predicate_protect <- function(x) {
  UseMethod("predicate_protect")
}
#' @export
predicate_protect.local_predicate <- function(x) {
  environment(x)[["protect"]]
}
#' @export
predicate_protect.global_predicate <- function(x) {
  attr(x, "protect_msg", exact = TRUE)
}

#' @export
`predicate_message<-` <- function(x, value) {
  UseMethod("predicate_message<-")
}
#' @export
`predicate_message<-.local_predicate` <- function(x, value) {
  environment(x)[["msg"]] <- value
  invisible(x)
}
#' @export
`predicate_message<-.global_predicate` <- function(x, value) {
  `attr<-`(x, "def_err_msg", value)
}

#' @export
`predicate_protect<-` <- function(x, value) {
  UseMethod("predicate_protect<-")
}
#' @export
`predicate_protect<-.local_predicate` <- function(x, value) {
  environment(x)[["protect"]] <- value
  invisible(x)
}
#' @export
`predicate_protect<-.global_predicate` <- function(x, value) {
  `attr<-`(x, "protect_msg", value)
}
