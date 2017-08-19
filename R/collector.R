#' Collect input validation checks
#'
#' @param ... Input-validation checks as error message-check definitions (cf.
#'   [rlang::dots_definitions()]).
#'
#' @return List of pairs of quosures, where each pair has components `msg`
#'   (error message) and `chk` (input validation check).
#'
#' @details The order of the input validation checks is not necessarily
#'   preserved.
#'
#' @examples
#' ## Use vld() to package input validation checks (which may be nested)
#' chks <- local({
#'   zero <- 0
#'   nonneg <- vld(
#'     {isTRUE(. >= zero)} ~
#'       vld(a - b, "b - c is negative ({b - c})" := b - c)
#'   )
#'   vld("Not numeric" := is.numeric, !!! nonneg)
#' })
#'
#' ## Enforce that a, b, c are numeric and a > b > c
#' f <- firmly(function(a, b, c) list(a, b, c), !!! chks)
#'
#' f(3, 2, 1)
#' \dontrun{
#' f(3, 1, 2)}
#'
#' @export
vld <- function(...) {
  dd <- dots_definitions(...)
  if (all(lengths(dd) == 0))
    return(NULL)
  vld_(dd$dots, dd$defs)
}

vld_ <- function(dots, defs) {
  as_vld(
    c(interp_message_check_pairs(dots), as_message_check_pairs(defs))
  )
}

interp_message_check_pairs <- function(dots) {
  lapply(dots, interp_message_check_pair)
}
interp_message_check_pair <- function(x) {
  x_eval <- try_eval_tidy(x)
  if (is.list(f_rhs(x)) || is_chkr_validation(x_eval))
    x_eval
  else
    set_empty_msg(x)
}
set_empty_msg <- function(x) {
  list(msg = quo(""), chk = x)
}

as_message_check_pairs <- function(defs) {
  lapply(defs, new_message_check_pair)
}
new_message_check_pair <- function(def) {
  rhs <- try_eval_tidy(def$rhs)
  if (is_chkr_validation(rhs)) {
    rhs$msg <- def$lhs
    rhs
  } else {
    names(def) <- nms_check[names(def)]
    def
  }
}
nms_check <- c(lhs = "msg", rhs = "chk")

as_vld <- make_as("validation_check")
is_vld <- check_is("validation_check")
