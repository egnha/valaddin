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

#' @rawNamespace exportPattern("^vld_.*$")
NULL

aliases <- paste(
  paste0("\\alias{", names(chkrs_purrr), "}"),
  collapse = "\n"
)
usage <- paste0(
  "\\usage{",
  paste(sprintf("%s(...)", sort(names(chkrs_purrr))), collapse = "\n\n"),
  "}"
)
#' Make local check formulae
#'
#' Check makers derived from purrr predicate functions.
#'
#' @evalRd aliases
#' @evalRd usage
#' @param ... Expressions of function arguments to check.
#' @return Check formula.
#' @name checkers
NULL