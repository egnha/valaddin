#' Capture input-validation checks
#'
#' @param ... Input-validation checks as error message-check definitions (cf.
#'   [rlang::dots_definitions()]).
#' @return List of pairs of quosures, with named components `msg` (error
#'   message), `chk` (input validation check).
#'
#' @examples
#' vld(!!!vld(!!!vld(f, a := b, c := d)), g, e := f)
#'
#' @export
vld <- function(...) {
  dd <- rlang::dots_definitions(...)
  c(splice_checks(dd$dots), name_checks(dd$defs))
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
