#' @include scope.R
NULL

scrub <- function(pttn_rm, pttn_sep) {
  force(pttn_rm)
  force(pttn_sep)

  function(x) gsub(pttn_sep, " ", gsub(pttn_rm, "", x))
}

localize_nm <- function(nm, what_is, ns, env) {
  msg <- paste("Not", what_is(nm))
  p <- getExportedValue(ns, nm)

  localize(f_new(p, msg, env = env))
}

make_vld_chkrs <- function(ns, pattern, sep, env = parent.frame()) {
  nms <- grep(pattern, getNamespaceExports(ns), value = TRUE)
  what_is <- scrub(pttn_rm = pattern, pttn_sep = sep)
  chkrs <- lapply(nms, localize_nm, ns = ns, what_is = what_is, env = env)
  names(chkrs) <- paste("vld", gsub(pattern, "", nms), sep = "_")

  chkrs[sort(names(chkrs))]
}

chkrs_purrr <- make_vld_chkrs("purrr", pattern = "^is_", sep = "_")
for (nm in names(chkrs_purrr))
  assign(nm, chkrs_purrr[[nm]])

#' @rawNamespace exportPattern("^vld_.*$")
NULL

nms <- lapply(c(bare = "^vld_bare", scalar = "^vld_scalar"),
              grep, x = names(chkrs_purrr), value = TRUE)
nms$misc  <- paste0("vld_", c("empty", "formula"))
nms$types <- setdiff(names(chkrs_purrr), unlist(nms))

#' Type checkers
#'
#' @evalRd rd_alias(nms$type)
#' @evalRd rd_usage(nms$type)
#' @param ... Expressions to check.
#' @return Check formula.
#' @family type checkers
#' @name checkers-type
NULL

#' Bare type checkers
#'
#' @evalRd rd_alias(nms$bare)
#' @evalRd rd_usage(nms$bare)
#' @param ... Expressions to check.
#' @inherit checkers-type
#' @family type checkers
#' @name checkers-bare-type
NULL

#' Scalar type checkers
#'
#' @evalRd rd_alias(nms$scalar)
#' @evalRd rd_usage(nms$scalar)
#' @param ... Expressions to check.
#' @inherit checkers-type
#' @family type checkers
#' @name checkers-scalar-type
NULL

#' Miscellaneous type checkers
#'
#' @evalRd rd_alias(nms$misc)
#' @evalRd rd_usage(nms$misc)
#' @param ... Expressions to check.
#' @inherit checkers-type
#' @family type checkers
#' @name checkers-misc
NULL
