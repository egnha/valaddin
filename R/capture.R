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
    c(splice_or_parse_dots(dd$dots), standardize_defs(dd$defs)),
    class = "validation_checks"
  )
}

splice_or_parse_dots <- function(dots) {
  lapply(dots, function(.) {
    x <- try_eval_tidy(.)
    if (is_spliceable(., x)) x else parse_dot(., x)
  })
}
is_spliceable <- function(., x) {
  is.list(rlang::f_rhs(.)) || inherits(x, "local_validation_checks")
}
parse_dot <- function(., x) {
  if (is_local_predicate(x)) globalize(x) else set_empty_msg(.)
}
set_empty_msg <- function(x) {
  list(msg = rlang::quo(""), chk = x)
}

standardize_defs <- function(defs) {
  lapply(defs, `names<-`, c("msg", "chk"))
}

is_vld <- function(x) {
  inherits(x, "validation_checks")
}
