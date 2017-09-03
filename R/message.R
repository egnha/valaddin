#' Get or set a validation error message
#'
#' @param f Predicate function.
#'
#' @return Quosure of a string.
#'
#' @examples
#' is_integer <- rlang::as_closure(is.integer)
#' vld_error_msg(is_integer) <- "{{.}} not of integer type (type: {typeof(.)})"
#' vld_error_msg(is_integer)
#'
#' foo <- firmly(identity, is_integer)
#' foo(1:3)
#' \dontrun{
#' foo(runif(3))}
#'
#' is_integer <- rlang::is_integer
#' msg <- local({
#'   len <- function(n) if (is.null(n)) "" else paste(" of length", n)
#'   new_vld_error_msg("{{.}} is not an integer vector{{len(.value$n)}}")
#' })
#' vld_error_msg(is_integer) <- msg
#'
#' foo <- firmly(identity, is_integer(n = 3))
#' foo(1:3)
#' \dontrun{
#' foo(1:2)}
#'
#' @export
vld_error_msg <- function(f) {
  environment(f)$`__valaddin_error_message` %||% empty_msg
}

#' @param env Environment that is in scope when a `\{\{...\}\}` substring of the
#'   error message is interpolated.
#' @param value Error message (string or [quosure][rlang::quosure] of a
#'   string).
#'
#' @details An error message can only be set for predicate functions that are
#'   [closures][base::function]. To set an error message of a
#'   [primitive][base::Primitive] predicate, e.g., `is.array()`, transform it to
#'   a closure with [rlang::as_closure()].
#'
#' @export
#' @rdname vld_error_msg
`vld_error_msg<-` <- function(f, env = parent.frame(), value) {
  if (!is_closure(f))
    abort("Can only set error message for predicates that are closures")
  if (!is.environment(env))
    abort("'env' must be an environment")
  if (is_string(value))
    msg <- new_quosure(value, env)
  else if (is_quosure(value) && is_string(f_rhs(value)))
    msg <- value
  else
    abort("Error message must be a string or quosure (of a string)")
  env_msg <- new.env(parent = environment(f))
  env_msg$`__valaddin_error_message` <- msg
  environment(f) <- env_msg
  invisible(f)
}

#' @param msg Error message (string).
#'
#' @export
#' @rdname vld_error_msg
new_vld_error_msg <- function(msg, env = parent.frame()) {
  if (!is_string(msg))
    abort("Error message much be a string")
  if (!is.environment(env))
    abort("'env' must be an environment")
  new_quosure(msg, env)
}
