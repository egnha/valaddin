#' Call a function against a given call signature
#'
#' @param .f Function.
#' @return Function of a call (as a language object).
#' @keywords internal
call_fn <- function(.f) {
  force(.f)

  function(.call) {
    .call[[1L]] <- .f
    .call
  }
}

#' Rewrite the argument signature of a function
#'
#' @param .f Function.
#' @param .sig Pairlist.
#' @param .attrs Named list (of attributes).
#' @return Function.
#' @keywords internal
with_sig <- function(.f, .sig, .attrs) {
  f <- eval(call("function", .sig, body(.f)))
  environment(f) <- environment(.f)
  attributes(f)  <- .attrs

  f
}