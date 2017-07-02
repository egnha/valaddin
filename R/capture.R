#' Capture input-validation checks
#'
#' @param ... Input-validation checks as error message-check definitions (cf.
#'   [rlang::dots_definitions()]).
#' @return List of pairs of quosures, with named components `msg` (error
#'   message) and `chk` (input validation check), of class `validationChecks`.
#'
#' @examples
#' # Stipulate that a, b, c are numeric and a > b > c
#' z <- 0
#' vld(is.numeric, !!! vld({isTRUE(. > !! z)} ~ vld(a - b, b - c)))
#'
#' @export
vld <- function(...) {
  dd <- rlang::dots_definitions(...)
  structure(
    c(splice_checks(dd$dots), name_checks(dd$defs)),
    class = "validationChecks"
  )
}

is_vld <- function(x) {
  inherits(x, "validationChecks")
}

name_checks <- function(defs) {
  lapply(defs, `names<-`, c("msg", "chk"))
}

splice_checks <- function(dots) {
  is_chk <- vapply(dots, is_check, logical(1))
  dots_splice <- lapply(dots[is_chk], rlang::eval_tidy)
  dots_checks <- lapply(dots[!is_chk], set_empty_msg)
  c(dots_checks, dots_splice)
}
is_check <- function(x) {
  is.list(rlang::f_rhs(x))
}
set_empty_msg <- function(x) {
  list(msg = rlang::quo(""), chk = x)
}
