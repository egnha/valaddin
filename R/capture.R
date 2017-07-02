#' Capture input validation checks
#'
#' @details The order of the input validation checks is not necessarily
#'   preserved.
#' @param ... Input-validation checks as error message-check definitions (cf.
#'   [rlang::dots_definitions()]).
#' @return List of pairs of quosures, with named components `msg` (error
#'   message) and `chk` (input validation check), of class `validationChecks`.
#'
#' @examples
#' # Stipulate that a, b, c are numeric and a > b > c
#' z <- 0
#' vld("Not numeric" := is.numeric,
#'     !!! vld({isTRUE(. > !! z)} ~ vld(a - b, b - c)))
#'
#' @export
vld <- function(...) {
  dd <- rlang::dots_definitions(...)
  structure(
    c(splice_checks(dd$dots), name_checks(dd$defs)),
    class = "validation_checks"
  )
}

name_checks <- function(defs) {
  lapply(defs, `names<-`, c("msg", "chk"))
}

splice_checks <- function(dots) {
  lapply(dots, function(.) {
    rhs <- rlang::f_rhs(.)
    x <- try_eval_tidy(.)
    if (is.list(rhs) || is_lcl_chk(x))
      x
    else
      set_empty_msg(.)
  })
}
is_lcl_chk <- function(x) {
  inherits(x, "local_validation_checks")
}
set_empty_msg <- function(x) {
  list(msg = rlang::quo(""), chk = x)
}

is_vld <- function(x) {
  inherits(x, "validation_checks")
}
