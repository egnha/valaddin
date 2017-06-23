assign_comparisons <- function(prefix, comparisons) {
  env <- parent.frame()
  for (cmp in comparisons) {
    nm <- paste0(prefix, cmp[[1]])
    assign(nm, localize_comparison(UQ(cmp[[2]]), cmp[[3]]), envir = env)
  }
}

comparisons <- list(
  boolean  = NULL,
  equiv    = NULL,
  pattern  = NULL,
  relation = NULL,
  sets     = NULL
)
comparisons$boolean <- list(
  list(
    "all_map",
    function(., .ref, ...)
      all(vapply(., rlang::as_function(.ref), logical(1)), ...),
    "{{.}} is not all true when mapped by '{{{.ref$expr}}}'"
  ),
  list(
    "any_map",
    function(., .ref, ...)
      any(vapply(., rlang::as_function(.ref), logical(1)), ...),
    "{{.}} is all false when mapped by '{{{.ref$expr}}}'"
  )
)
comparisons$equiv <- list(
  list(
    "all_equal",
    function(target, current, ...)
      isTRUE(all.equal(target, current, ...)),
    "{{.}} does not equal '{{{.ref$expr}}}'"
  ),
  list(
    "identical",
    identical,
    "{{.}} is not identical to '{{{.ref$expr}}}'"
  )
)
comparisons$pattern <- list(
  list(
    "grepl",
    function(x, pattern, ...)
      isTRUE(grepl(pattern, x, ...)),
    "Pattern '{{{.ref$value}}}' is not matched in {{.}}"
  ),
  list(
    "inherits",
    inherits,
    "{{.}} is not of class \"{{{.ref$value}}}\""
  )
)
comparisons$relation <- list(
  list(
    "gt",
    quote({isTRUE(. > .ref)}),
    "{{.}} is not greater than {{{.ref$value}}}"
  ),
  list(
    "gte",
    quote({isTRUE(. >= .ref)}),
    "{{.}} is not greater than or equal to {{{.ref$value}}}"
  ),
  list(
    "lt",
    quote({isTRUE(. < .ref)}),
    "{{.}} is not less than {{{.ref$value}}}"
  ),
  list(
    "lte",
    quote({isTRUE(. <= .ref)}),
    "{{.}} is not less than or equal to {{{.ref$value}}}"
  ),
  list(
    "near",
    quote({isTRUE(abs(. - .ref) < sqrt(.Machine$double.eps))}),
    "{{.}} does not (nearly) equal {{{.ref$value}}}"
  )
)
comparisons$sets <- list(
  list(
    "in",
    quote({isTRUE(. %in% .ref)}),
    "{{.}} is not in '{{{.ref$expr}}}'"
  ),
  list(
    "not_in",
    quote({isTRUE(! . %in% .ref)}),
    "{{.}} is in '{{{.ref$expr}}}'"
  ),
  list(
    "contains",
    quote({all(.ref %in% .)}),
    "{{.}} does not contain '{{{.ref$expr}}}'"
  ),
  list(
    "doesnt_contain",
    quote({any(! .ref %in% .)}),
    "{{.}} contains '{{{.ref$expr}}}'"
  ),
  list(
    "contained_in",
    quote({all(. %in% .ref)}),
    "{{.}} is not contained in '{{{.ref$expr}}}'"
  ),
  list(
    "not_contained_in",
    quote({any(! . %in% .ref)}),
    "{{.}} is contained in '{{{.ref$expr}}}'"
  ),
  list(
    "intersects",
    quote({length(intersect(., .ref)) > 0}),
    "{{.}} does not intersect '{{{.ref$expr}}}'"
  ),
  list(
    "doesnt_intersect",
    quote({length(intersect(., .ref)) == 0}),
    "{{.}} intersects '{{{.ref$expr}}}'"
  ),
  list(
    "setequal",
    setequal,
    "{{.}} and '{{{.ref$expr}}}' are not equal as sets"
  )
)

assign_comparisons("vld_", unlist(comparisons, recursive = FALSE))

#' @rawNamespace exportPattern("^vld_.*$")
NULL

# Non-bare "numerical" predicates are omitted from rlang 0.1.1
is_numeric <- function(x, n = NULL) {
  if (!typeof(x) %in% c("double", "integer"))
    return(FALSE)
  if (!is.null(n) && length(x) != n)
    return(FALSE)
  TRUE
}
is_scalar_numeric <- function(x) {
  is_numeric(x) && length(x) == 1
}
is_number <- is_scalar_numeric
