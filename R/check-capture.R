#' Capture input validation checks
#'
#' @param ... Input-validation checks as error message-check definitions (cf.
#'   [rlang::dots_definitions()]).
#' @return List of pairs of quosures, with named components `msg` (error
#'   message) and `chk` (input validation check), of class `validation_checks`.
#'
#' @details The order of the input validation checks is not necessarily
#'   preserved.
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
    vld_(dd$dots, dd$defs),
    class = "validation_checks"
  )
}

vld_ <- function(dots, defs) {
  c(splice_or_parse_dots(dots), standardize_defs(defs))
}

splice_or_parse_dots <- function(dots) {
  lapply(dots, function(.) {
    .eval <- try_eval_tidy(.)
    if (is_spliceable(., .eval)) .eval else set_empty_msg(.)
  })
}
is_spliceable <- function(., .eval) {
  is.list(rlang::f_rhs(.)) ||
    is_global_predicate(.eval) ||
    is_local_validation_checks(.eval)
}
is_global_predicate <- identify_class("global_predicate")
is_local_validation_checks <- identify_class("local_validation_checks")
set_empty_msg <- function(x) {
  list(msg = rlang::quo(""), chk = x)
}

standardize_defs <- function(defs) {
  lapply(defs, `names<-`, c("msg", "chk"))
}

is_vld <- identify_class("validation_checks")
