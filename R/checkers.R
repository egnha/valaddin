#' @include scope.R
NULL

localize_nm <- function(nm, what_is, ns, env) {
  msg <- paste("Not", what_is(nm))
  p <- getExportedValue(ns, nm)

  localize_check(f_new(p, msg, env = env))
}

scrub <- function(ptrn_rm, ptrn_sep) {
  force(ptrn_rm); force(ptrn_sep)

  function(x) gsub(ptrn_sep, " ", gsub(ptrn_rm, "", x))
}

make_vld_chkrs <- function(ns, pattern, sep, env = parent.frame()) {
  nms <- grep(pattern, getNamespaceExports(ns), value = TRUE)
  what_is <- scrub(ptrn_rm = pattern, ptrn_sep = sep)
  chkrs <- lapply(nms, localize_nm, ns = ns, what_is = what_is, env = env)
  names(chkrs) <- paste("vld", gsub(pattern, "", nms), sep = "_")

  chkrs
}

chkrs_purrr <- make_vld_chkrs("purrr", pattern = "^is_", sep = "_")
for (nm in names(chkrs_purrr))
  assign(nm, chkrs_purrr[[nm]])

#' Make local check formulae
#'
#' Check makers derived from purrr predicate functions.
#'
#' @evalRd rd_alias(names(chkrs_purrr))
#' @evalRd rd_usage(c(names(chkrs_purrr)))
#' @param ... Expressions of function arguments to check.
#' @return Check formula.
#'
#' @rawNamespace exportPattern("^vld_.*$")
#' @name checkers
NULL
