localize_nm <- function(nm, what_is, ns, env) {
  msg <- paste("Not", what_is(nm))
  p <- getExportedValue(ns, nm)

  # Cannot use lazyeval::f_new (<= 0.2.0) because p is not a language object
  localize(ff_new(p, msg, env = env))
}

replace <- function(x, pattern, with, ...) gsub(pattern, with, x, ...)
delete  <- function(x, pattern, ...) replace(x, pattern, "", ...)
scrub   <- function(pttn_rm, pttn_sep) {
  force(pttn_rm)
  force(pttn_sep)

  function(x)
    x %>% delete(pttn_rm) %>% replace(pttn_sep, with = " ")
}

make_vld_chkrs <- function(nms, pattern, sep, ns, env = parent.frame()) {
  what_is <- scrub(pttn_rm = pattern, pttn_sep = sep)
  chkrs <- lapply(nms, localize_nm, ns = ns, what_is = what_is, env = env)
  names(chkrs) <- nms %>%
    delete(pattern) %>%
    replace("\\.", with = "_") %>% {
      paste("vld", ., sep = "_")
    }

  chkrs[sort(names(chkrs))]
}

# purrr::is_*numeric() are deprecated in purrr (>= 0.2.2.9000)
re_is_not_numeric <- "^is_((?!numeric).)*$"

pkg <- list(
  base = list(
    nms = c(
      "is.array",       "is.call",        "is.complex",     "is.data.frame",
      "is.environment", "is.expression",  "is.factor",      "is.language",
      "is.matrix",      "is.na",          "is.name",        "is.nan",
      "is.ordered",     "is.pairlist",    "is.primitive",   "is.raw",
      "is.recursive",   "is.symbol",      "is.table",       "is.unsorted"
    ),
    pattern = "^is\\.",
    sep = "\\.",
    ns = "base"
  ),
  purrr = list(
    nms = grep(re_is_not_numeric, getNamespaceExports("purrr"),
               value = TRUE, perl = TRUE),
    pattern = "^is_",
    sep = "_",
    ns = "purrr"
  )
)

chkrs_ <- pkg %>%
  lapply(function(.) make_vld_chkrs(.$nms, .$pattern, .$sep, .$ns))
chkrs <- do.call("c", unname(chkrs_))

# "numeric" has conflicting interpretations in base R, so treat it differently
chkrs$vld_numeric <- localize("Not double/integer" ~ is.numeric)
chkrs$vld_scalar_numeric <- localize(
  "Not scalar double/integer" ~ {is.numeric(.) && length(.) == 1L}
)

# Aliases
replace_msg <- function(chkr, msg) {
  f <- globalize(chkr)
  ff_lhs(f) <- msg
  localize(f)
}

chkrs_alias <- list(
  "Not boolean"   = chkrs$vld_scalar_logical,
  "Not number"    = chkrs$vld_scalar_numeric,
  "Not string"    = chkrs$vld_scalar_character,
  "Not singleton" = chkrs$vld_scalar_vector
) %>%
  Map(replace_msg, ., names(.)) %>%
  `names<-`(replace(names(.), "^Not ", "vld_"))

chkrs <- c(chkrs, chkrs_alias)

for (nm in names(chkrs))
  assign(nm, chkrs[[nm]])

#' @rawNamespace exportPattern("^vld_.*$")
NULL

# Documentation -----------------------------------------------------------

# Aliases, "Usage"

nms <- lapply(c(bare = "^vld_bare", scalar = "^vld_scalar"),
              grep, x = names(chkrs_$purrr), value = TRUE, perl = TRUE)
nms$misc <- c(
  names(chkrs_$base),
  paste0("vld_", c("empty", "formula", "numeric", "scalar_numeric", "number"))
)
nms$type <- setdiff(names(chkrs_$purrr), unlist(nms))
nms <- lapply(nms, function(.) .[order(tolower(.))])
nms$scalar <- c(nms$scalar, setdiff(names(chkrs_alias), "vld_number"))

# "See Also" (required because @family would overwrite our custom "See Also")

gather <- function(x) gsub("\n([^\n])", " \\1", x)
tidy <- function(x) gsub(" \n|\n ", "\n", gsub(" +", " ", x))
trim <- function(x) trimws(tidy(gather(x)), which = "both")

link_bare  <- "\\link{%s}"
link_extfn <- "\\code{\\link[%s]{%s}}"
link_purrr <- trim("
  \\link[purrr:%s-predicates]{%s predicates}
  (\\href{https://cran.r-project.org/package=purrr}{\\pkg{purrr}}
")

prefix_with <- function(x, text) {
  stats::setNames(paste(text, x), names(x)) %>% as.list()
}

predicates <- list(
  misc = sprintf(link_extfn, "base", sort(c(pkg$base$nms, "is.numeric"))) %>%
    c(sprintf(link_extfn, "purrr", c("is_empty", "is_formula"))) %>%
    paste(collapse = ", "),
  bare   = sprintf(link_purrr, "bare-type", "Bare type"),
  scalar = sprintf(link_purrr, "scalar-type", "Scalar type"),
  type   = sprintf(link_purrr, "type", "Type")
) %>%
  prefix_with("Corresponding predicates:")

other <- list(misc = NA, bare = NA, scalar = NA, type = NA)
other[] <- trim("
  \\code{\\link{globalize}} recovers the underlying check formula of global
  scope.
  \n\n
  The notion of \\dQuote{scope} is explained in the \\emph{Check Formulae}
  section of \\link{firmly}.
")
tmpl <- trim("
  \\code{\\link{vld_%s}} does not check according to type, and is not based on
  \\code{purrr::%s} (deprecated since 0.2.2.9000); rather, it is based on the
  predicate \\code{%s}, which checks whether an object is \\dQuote{numerical} in
  the sense of \\code{\\link[base]{mode}} instead of
  \\code{\\link[base]{typeof}}. In particular, factors are not regarded as
  \\dQuote{numerical}.
")
other$type <- paste(
  other$type,
  sprintf(tmpl, "numeric", "is_numeric", "\\link[base]{is.numeric}"),
  sep = "\n\n"
)
other$scalar <- paste(
  other$scalar,
  sprintf(tmpl, "scalar_numeric", "is_scalar_numeric",
          "function(x) is.numeric(x) && length(x) == 1L"),
  sep = "\n\n"
)

doc_nms <- c("misc", "bare-type", "scalar-type", "type")
family <- doc_nms %>%
  stats::setNames(paste(., "checkers", sep = "-"), .) %>%
  lapply(function(nm) {
    other <- unname(.[. != nm])
    paste(sprintf(link_bare, other), collapse = ", ")
  }) %>%
  prefix_with("Other checkers:")

ref <- Map(c, predicates, other, family)

#' Miscellaneous checkers
#'
#' These functions make check formulae of local scope based on the
#' correspondingly named \pkg{base} R predicates \code{is.*} (e.g.,
#' \code{vld_data_frame} corresponds to the predicate
#' \code{\link[base]{is.data.frame}}), with the following exceptions:
#' \code{vld_empty}, \code{vld_formula}, is based on the
#' \href{https://cran.r-project.org/package=purrr}{\pkg{purrr}} predicate
#' \code{\link[purrr]{is_empty}}, \code{\link[purrr]{is_formula}}, resp., and
#' \code{vld_number} is an alias for \code{vld_scalar_numeric}, which is based
#' on the predicate \code{function(x) is.numeric(x) && length(x) == 1L}.
#'
#' @evalRd rd_alias(nms$misc)
#' @evalRd rd_usage(nms$misc)
#' @param \dots One-sided formulae that specify the expressions to check.
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
#' g <- firmly(f, vld_formula(~ x))
#' g(z ~ a + b, 0)  # "Pass"
#' g(0, 0)          # Error: "Not formula: x"
#'
#' # Impose the condition that x and y are disjoint (assuming they are vectors)
#' h <- firmly(f, vld_empty(~ intersect(x, y)))
#' h(letters[1:3], letters[4:5])  # "Pass"
#' h(letters[1:3], letters[3:5])  # Error: "Not empty: intersect(x, y)"
#' }
#' @name misc-checkers
NULL

#' Type checkers
#'
#' These functions make check formulae of local scope based on the
#' correspondingly named \link[purrr:type-predicates]{type predicate} from the
#' \href{https://cran.r-project.org/package=purrr}{\pkg{purrr}}
#' package. For example, \code{vld_atomic} creates check formulae (of local
#' scope) for the \pkg{purrr} predicate function
#' \code{\link[purrr]{is_atomic}}.
#'
#' @evalRd rd_alias(nms$type)
#' @evalRd rd_usage(nms$type)
#' @param \dots One-sided formulae that specify the expressions to check.
#' @inherit misc-checkers
#' @evalRd rd_seealso(ref$type)
#' @examples
#' \dontrun{
#'
#' f <- function(x, y) "Pass"
#'
#' # Impose a check on x: ensure it's of type "logical"
#' f_firm <- firmly(f, vld_logical(~x))
#' f_firm(TRUE, 0)  # "Pass"
#' f_firm(1, 0)     # Error: "Not logical: x"
#'
#' # To impose the same check on all arguments, apply globalize()
#' f_firmer <- firmly(f, globalize(vld_logical))
#' f_firmer(TRUE, FALSE)  # "Pass"
#' f_firmer(TRUE, 0)      # Error: "Not logical: y"
#' f_firmer(1, 0)         # Errors: "Not logical: x", "Not logical: y"
#' }
#' @name type-checkers
NULL

#' Bare type checkers
#'
#' These functions make check formulae of local scope based on the
#' correspondingly named \link[purrr:bare-type-predicates]{bare type predicate}
#' from the
#' \href{https://cran.r-project.org/package=purrr}{\pkg{purrr}}
#' package. For example, \code{vld_bare_atomic} creates check formulae (of
#' local scope) for the \pkg{purrr} predicate function
#' \code{\link[purrr]{is_bare_atomic}}.
#'
#' @evalRd rd_alias(nms$bare)
#' @evalRd rd_usage(nms$bare)
#' @param \dots One-sided formulae that specify the expressions to check.
#' @inherit misc-checkers
#' @evalRd rd_seealso(ref$bare)
#' @examples
#' \dontrun{
#'
#' f <- function(x, y) "Pass"
#'
#' # Impose a check on x: ensure it's a bare logical object (i.e., has no class)
#' f_firm <- firmly(f, vld_bare_logical(~x))
#' x <- structure(TRUE, class = "boolean")
#' f_firm(TRUE, 0)  # "Pass"
#' f_firm(x, 0)     # Error: "Not bare logical: x"
#'
#' # To impose the same check on all arguments, apply globalize()
#' f_firmer <- firmly(f, globalize(vld_bare_logical))
#' f_firmer(TRUE, FALSE)  # "Pass"
#' f_firmer(TRUE, 0)      # Error: "Not bare logical: y"
#' f_firmer(x, 0)         # Errors: "Not bare logical: x", "Not bare logical: y"
#' }
#' @name bare-type-checkers
NULL

#' Scalar type checkers
#'
#' These functions make check formulae of local scope based on the
#' correspondingly named \link[purrr:scalar-type-predicates]{scalar type
#' predicate} from the
#' \href{https://cran.r-project.org/package=purrr}{\pkg{purrr}}
#' package. For example, \code{vld_scalar_atomic} creates check formulae (of
#' local scope) for the predicate function
#' \code{\link[purrr]{is_scalar_atomic}}. \cr\cr The functions
#' \code{vld_boolean}, \code{vld_number}, \code{vld_string},
#' \code{vld_singleton} are aliases for \code{vld_scalar_logical},
#' \code{vld_scalar_numeric}, \code{vld_scalar_character},
#' \code{vld_scalar_vector}, resp. (with appropriately modified error messages).
#'
#' @evalRd rd_alias(nms$scalar)
#' @evalRd rd_usage(nms$scalar)
#' @param \dots One-sided formulae that specify the expressions to check.
#' @inherit misc-checkers
#' @evalRd rd_seealso(ref$scalar)
#' @examples
#' \dontrun{
#'
#' f <- function(x, y) "Pass"
#'
#' # Impose a check on x: ensure it's boolean (i.e., a scalar logical vector)
#' f_firm <- firmly(f, vld_boolean(~x))
#' f_firm(TRUE, 0)           # "Pass"
#' f_firm(c(TRUE, TRUE), 0)  # Error: "Not boolean: x"
#'
#' # To impose the same check on all arguments, apply globalize
#' f_firmer <- firmly(f, globalize(vld_boolean))
#' f_firmer(TRUE, FALSE)    # "Pass"
#' f_firmer(TRUE, 0)        # Error: "Not boolean: y"
#' f_firmer(logical(0), 0)  # Errors: "Not boolean: x", "Not boolean: y"
#' }
#' @name scalar-type-checkers
NULL
