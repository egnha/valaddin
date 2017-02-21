#' Capture function-call arguments as promises
#'
#' @param .call Function call (language).
#' @param .sig Function argument signature (pairlist).
#' @param .env Environment in which the function-call arguments are to be
#'   (lazily) evaluated.
#'
#' @return Environment in which the function-call arguments (and no other
#'   expressions) are bound as promises.
#'
#' @details This is essentially equivalent to using
#'   \code{\link[base]{delayedAssign}()} in a for-loop to bind promises, but at
#'   roughly twice the speed and half the code.
#'
#' @noRd
promises <- function(.call, .sig, .env) {
  .call[[1L]] <- eval(call("function", .sig, quote(environment())))
  eval(.call, .env)
}
