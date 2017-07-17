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
    x <- try_eval_tidy(.)
    if (is_spliceable(., x)) x else parse_dot(., x)
  })
}
is_spliceable <- function(., x) {
  is.list(rlang::f_rhs(.)) || is_lcl_vld_chks(x)
}
is_lcl_vld_chks <- identify_class("local_validation_checks")
parse_dot <- function(., x) {
  if (is_local_predicate(x)) globalize(x) else set_empty_msg(.)
}
set_empty_msg <- function(x) {
  list(msg = rlang::quo(""), chk = x)
}

standardize_defs <- function(defs) {
  lapply(defs, `names<-`, c("msg", "chk"))
}

is_vld <- identify_class("validation_checks")
