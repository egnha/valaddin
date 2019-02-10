localize_nm <- function(nm, what_is, ns, env) {
  msg <- paste("Not", what_is(nm))
  p <- getExportedValue(ns, nm)
  localize(lazyeval::f_new(p, msg, env = env))
}

replace <- function(x, pattern, with, ...) {
  gsub(pattern, with, x, ...)
}
delete <- function(x, pattern, ...) {
  replace(x, pattern, "", ...)
}
scrub <- function(pattern_rm, pattern_sep) {
  function(x) {
    replace(delete(x, pattern_rm), pattern_sep, with = " ")
  }
}

make_vld_chkrs <- function(nms, pattern, sep, ns, env = parent.frame()) {
  what_is <- scrub(pattern, sep)
  chkrs <- lapply(nms, localize_nm, ns = ns, what_is = what_is, env = env)
  names(chkrs) <- nms %>%
    delete(pattern) %>%
    replace("\\.", with = "_") %>%
    paste("vld", ., sep = "_")
  chkrs[sort(names(chkrs))]
}

chkrs <- make_vld_chkrs(
  nms = c(
    "is.array",       "is.call",       "is.complex",   "is.data.frame",
    "is.environment", "is.expression", "is.factor",    "is.language",
    "is.matrix",      "is.name",       "is.null",      "is.numeric",
    "is.ordered",     "is.pairlist",   "is.primitive", "is.raw",
    "is.recursive",   "is.symbol",     "is.table",     "is.unsorted",
    "is.function",    "is.atomic",     "is.list",      "is.vector",
    "is.double",      "is.character",  "is.integer",   "is.logical"
  ),
  pattern = "^is\\.",
  sep = "\\.",
  ns = "base"
)

chkrs$vld_true <- localize(lazyeval::f_new(is_true, "Not TRUE"))

chkrs$vld_false <- localize(lazyeval::f_new(is_false, "Not FALSE"))

chkrs$vld_all <- localize(lazyeval::f_new(all, "Not all TRUE"))

chkrs$vld_any <- localize(lazyeval::f_new(any, "None TRUE"))

chkrs$vld_empty <- localize(lazyeval::f_new(
  quote({length(.) == 0L}), "Not empty"))

chkrs$vld_singleton <- localize(lazyeval::f_new(
  quote({length(.) == 1L}), "Not singleton"))

chkrs$vld_closure <- localize(lazyeval::f_new(
  quote({typeof(.) == "closure"}), "Not closure"))

chkrs$vld_formula <- localize(lazyeval::f_new(
  quote({typeof(.) == "language" && inherits(., "formula")}), "Not formula"))

chkrs$vld_na <- localize(lazyeval::f_new(quote({isTRUE(is.na(.))}), "Not NA"))

chkrs$vld_nan <- localize(lazyeval::f_new(quote({isTRUE(is.nan(.))}), "Not NaN"))

chkrs$vld_scalar_numeric <- localize(lazyeval::f_new(
  quote({is.numeric(.) && length(.) == 1L}), "Not scalar numeric"))

chkrs$vld_scalar_logical <- localize(lazyeval::f_new(
  quote({is.logical(.) && length(.) == 1L}), "Not scalar logical"))

chkrs$vld_scalar_integer <- localize(lazyeval::f_new(
  quote({is.integer(.) && length(.) == 1L}), "Not scalar integer"))

chkrs$vld_scalar_vector <- localize(lazyeval::f_new(
  quote({is.vector(.) && length(.) == 1L}), "Not scalar vector"))

chkrs$vld_scalar_atomic <- localize(lazyeval::f_new(
  quote({is.atomic(.) && length(.) == 1L}), "Not scalar atomic"))

chkrs$vld_scalar_list <- localize(lazyeval::f_new(
  quote({is.list(.) && length(.) == 1L}), "Not scalar list"))

chkrs$vld_scalar_double <- localize(lazyeval::f_new(
  quote({is.double(.) && length(.) == 1L}), "Not scalar double"))

chkrs$vld_scalar_complex <- localize(lazyeval::f_new(
  quote({is.complex(.) && length(.) == 1L}), "Not scalar complex"))

chkrs$vld_scalar_character <- localize(lazyeval::f_new(
  quote({is.character(.) && length(.) == 1L}), "Not scalar character"))

chkrs$vld_scalar_raw <- localize(lazyeval::f_new(
  quote({is.raw(.) && length(.) == 1L}), "Not scalar raw"))

# Aliases
replace_msg <- function(chkr, msg) {
  f <- globalize(chkr)
  lazyeval::f_lhs(f) <- msg
  localize(f)
}
chkrs_alias <- list(
  "Not boolean" = chkrs$vld_scalar_logical,
  "Not number"  = chkrs$vld_scalar_numeric,
  "Not string"  = chkrs$vld_scalar_character
) %>%
  Map(replace_msg, ., names(.)) %>%
  setNames(replace(names(.), "^Not ", "vld_"))

chkrs <- c(chkrs, chkrs_alias)
for (nm in names(chkrs))
  assign(nm, chkrs[[nm]])

#' @rawNamespace exportPattern("^vld_.*$")
NULL

# Documentation -----------------------------------------------------------

# Aliases, "Usage"

nms <- list()
nms$type <- paste0("vld_", c(
  "logical", "integer", "double", "complex", "character", "raw"
))
nms$scalar <- c(
  "vld_singleton", "vld_boolean", "vld_number", "vld_string",
  paste0("vld_scalar_", c(
    "logical", "integer", "double", "complex", "character", "raw",
    "numeric", "atomic", "vector", "list"
  ))
)
nms$misc <- setdiff(names(chkrs), c(nms$type, nms$scalar))
nms <- lapply(nms, sort)

# Ensure that all checkers are accounted for
stopifnot(setequal(names(chkrs), unlist(nms, use.names = FALSE)))

# "See Also" (required because @family would overwrite our custom "See Also")

trim <- local({
  gather <- function(x) {
    gsub("\n([^\n])", " \\1", x)
  }
  tidy <- function(x) {
    gsub(" \n|\n ", "\n", gsub(" +", " ", x))
  }
  function(x) {
    trimws(tidy(gather(x)), which = "both")
  }
})

prefix_with <- function(x, text) {
  as.list(setNames(paste(text, x), names(x)))
}

base_predicate <- function(x, prefix = "^vld") {
  sub(prefix, "is", gsub("_", ".", x))
}

join <- function(x) paste(x, collapse = ", ")

link <- list(
  bare = "\\link{%s}",
  ext  = "\\code{\\link[%s]{%s}}"
)

misc_predicates <- nms$misc %>%
  setdiff(c(
    "vld_all", "vld_any", "vld_empty", "vld_formula", "vld_closure",
    "vld_true", "vld_false"
  )) %>%
  base_predicate %>%
  c("all", "any")
scalar_predicates <- nms$scalar %>%
  setdiff(c("vld_boolean", "vld_number", "vld_singleton", "vld_string")) %>%
  base_predicate("^vld.scalar")
predicates <- list(
  type = sprintf(link$ext, "base", base_predicate(nms$type)),
  scalar = sprintf(link$ext, "base", sort(scalar_predicates)),
  misc = sprintf(link$ext, "base", sort(misc_predicates))
) %>%
  lapply(join) %>%
  prefix_with("Corresponding predicates:")

other <- trim("
  \\code{\\link{globalize}} recovers the underlying check formula of global
  scope.
  \n\n
  The notions of \\dQuote{scope} and \\dQuote{check item} are explained in the
  \\emph{Check Formulae} section of \\link{firmly}.
")

family <- c("type", "scalar", "misc") %>%
  setNames(paste(., "checkers", sep = "-"), .) %>%
  lapply(function(nm) {
    other <- unname(.[. != nm])
    join(sprintf(link$bare, other))
  }) %>%
  prefix_with("Other checkers:")

ref <- Map(c, predicates, other, family)

#' Miscellaneous checkers
#'
#' @description
#' These functions make check formulae of local scope based on the
#' correspondingly named \pkg{base} R predicates \code{is.*} (e.g.,
#' \code{vld_data_frame} corresponds to the predicate
#' \code{\link[base]{is.data.frame}}), with the following exceptions:
#'
#'   - `vld_empty` is based on the predicate `length(.) == 0`
#'
#'   - `vld_formula` is based on the predicate
#'     `typeof(.) == "language" && inherits(., "formula")`
#'
#'   - `vld_closure` is based on the predicate `typeof(.) == "closure"`
#'
#'   - `vld_true` and `vld_false` are based on the predicates
#'     `identical(., TRUE)` and `identical(., FALSE)`, resp.
#'
#' The checkers \code{vld_true} and \code{vld_false} are all-purpose checkers to
#' specify \emph{arbitrary} input validation checks.
#'
#' @evalRd rd_alias(nms$misc)
#' @evalRd rd_usage(nms$misc)
#' @param \dots Check items, i.e., formulae that are one-sided or have a string
#'   as left-hand side (see \emph{Check Formulae of Local Scope} in the
#'   documentation page \link{firmly}). These are the expressions to check.
#' @return Check formula of local scope.
#' @details Each function \code{vld_*} is a function of class
#'   \code{"check_maker"}, generated by \code{\link{localize}}.
#' @evalRd rd_seealso(ref$misc)
#' @examples
#' \dontrun{
#'
#' f <- function(x, y) "Pass"
#'
#' # Impose the condition that x is a formula
#' g <- firmly(f, vld_formula(~x))
#' g(z ~ a + b, 0)  # [1] "Pass"
#' g(0, 0)          # Error: "Not formula: x"
#'
#' # Impose the condition that x and y are disjoint (assuming they are vectors)
#' h <- firmly(f, vld_empty(~intersect(x, y)))
#' h(letters[1:3], letters[4:5])  # [1] "Pass"
#' h(letters[1:3], letters[3:5])  # Error: "Not empty: intersect(x, y)"
#'
#' # Use a custom error message
#' h <- firmly(f, vld_empty("x, y must be disjoint" ~ intersect(x, y)))
#' h(letters[1:3], letters[3:5])  # Error: "x, y must be disjoint"
#'
#' # vld_true can be used to implement any kind of input validation
#' ifelse_f <- firmly(ifelse, vld_true(~typeof(yes) == typeof(no)))
#' (w <- {set.seed(1); rnorm(5)})
#' # [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078
#' ifelse_f(w > 0, 0, "1")  # Error: "Not TRUE: typeof(yes) == typeof(no)"
#' ifelse_f(w > 0, 0, 1)    # [1] 1 0 1 0 0
#' }
#'
#' @name misc-checkers
NULL

#' Type checkers
#'
#' These functions make check formulae of local scope based on the
#' correspondingly named (atomic) type predicate from \pkg{base} R.
#'
#' @evalRd rd_alias(nms$type)
#' @evalRd rd_usage(nms$type)
#' @param \dots Check items, i.e., formulae that are one-sided or have a string
#'   as left-hand side (see \emph{Check Formulae of Local Scope} in the
#'   documentation page \link{firmly}). These are the expressions to check.
#' @inherit misc-checkers
#' @evalRd rd_seealso(ref$type)
#' @examples
#' \dontrun{
#'
#' f <- function(x, y) "Pass"
#'
#' # Impose a check on x: ensure it's of type "logical"
#' f_firm <- firmly(f, vld_logical(~x))
#' f_firm(TRUE, 0)  # [1] "Pass"
#' f_firm(1, 0)     # Error: "Not logical: x"
#'
#' # Use a custom error message
#' f_firm <- firmly(f, vld_logical("x should be a logical vector" ~ x))
#' f_firm(1, 0)     # Error: "x should be a logical vector"
#'
#' # To impose the same check on all arguments, apply globalize()
#' f_firmer <- firmly(f, globalize(vld_logical))
#' f_firmer(TRUE, FALSE)  # [1] "Pass"
#' f_firmer(TRUE, 0)      # Error: "Not logical: `y`"
#' f_firmer(1, 0)         # Errors: "Not logical: `x`", "Not logical: `y`"
#' }
#' @name type-checkers
NULL

#' Scalar checkers
#'
#' @description These functions make check formulae of local scope based on the
#' correspondingly named scalar type predicate from \pkg{base} R. For example,
#' `vld_scalar_logical` creates check formulae (of local scope) for the
#' predicate `is.logical(.) && length(.) == 1`. The function `vld_singleton` is
#' based on the predicate `length(.) == 1`.
#'
#' The functions `vld_boolean`, `vld_number`, `vld_string` are aliases for
#' `vld_scalar_logical`, `vld_scalar_numeric`, `vld_scalar_character`, resp.
#' (with appropriately modified error messages).
#'
#' @evalRd rd_alias(nms$scalar)
#' @evalRd rd_usage(nms$scalar)
#' @param \dots Check items, i.e., formulae that are one-sided or have a string
#'   as left-hand side (see \emph{Check Formulae of Local Scope} in the
#'   documentation page \link{firmly}). These are the expressions to check.
#' @inherit misc-checkers
#' @evalRd rd_seealso(ref$scalar)
#' @examples
#' \dontrun{
#'
#' f <- function(x, y) "Pass"
#'
#' # Impose a check on x: ensure it's boolean (i.e., a scalar logical vector)
#' f_firm <- firmly(f, vld_boolean(~x))
#' f_firm(TRUE, 0)           # [1] "Pass"
#' f_firm(c(TRUE, TRUE), 0)  # Error: "Not boolean: x"
#'
#' # Use a custom error message
#' f_firm <- firmly(f, vld_boolean("x is not TRUE/FALSE/NA" ~ x))
#' f_firm(c(TRUE, TRUE), 0)  # Error: "x is not TRUE/FALSE/NA"
#'
#' # To impose the same check on all arguments, apply globalize
#' f_firmer <- firmly(f, globalize(vld_boolean))
#' f_firmer(TRUE, FALSE)    # [1] "Pass"
#' f_firmer(TRUE, 0)        # Error: "Not boolean: `y`"
#' f_firmer(logical(0), 0)  # Errors: "Not boolean: `x`", "Not boolean: `y`"
#' }
#' @name scalar-checkers
NULL
