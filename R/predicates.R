comparisons <- list(
  boolean  = NULL,
  pattern  = NULL,
  property = NULL,
  relation = NULL,
  sets     = NULL
)
comparisons$boolean <- list(
  list(
    "all_map",
    function(., .ref, ...)
      all(vapply(., rlang::as_function(.ref), logical(1)), ...),
    "{{.}} is not all true when mapped by {{{.ref$expr}}}"
  ),
  list(
    "any_map",
    function(., .ref, ...)
      any(vapply(., rlang::as_function(.ref), logical(1)), ...),
    "{{.}} is all false when mapped by {{{.ref$expr}}}"
  )
)
comparisons$pattern <- list(
  list(
    "grepl",
    function(., .ref, ...)
      isTRUE(grepl(.ref, ., ...)),
    'Pattern "{{{.ref$value}}}" is not matched in {{.}}'
  )
)
comparisons$property <- list(
  list(
    "has_name",
    quote({.ref %in% names(.)}),
    "{{.}} does not have name {{{.ref$value}}}"
  ),
  list(
    "has_names",
    quote({all(.ref %in% names(.))}),
    "{{.}} does not have names {{{.ref$value}}}"
  ),
  list(
    "has_length",
    quote({length(.) == .ref}),
    "{{.}} is not of length {{{.ref$value}}}"
  ),
  list(
    "has_attr",
    quote({.ref %in% attributes(.)}),
    "{{.}} does not have attribute {{{.ref$value}}}"
  ),
  list(
    "has_attrs",
    quote({all(.ref %in% attributes(.))}),
    "{{.}} does not have attributes {{{.ref$value}}}"
  ),
  list(
    "inherits",
    inherits,
    '{{.}} is not of class "{{{.ref$value}}}"'
  )
)
comparisons$relation <- list(
  list(
    "identical",
    identical,
    "{{.}} is not identical to {{{.ref$expr}}}"
  ),
  list(
    "not_identical",
    quote({!identical(., .ref, ...)}),
    "{{.}} is identical to {{{.ref$expr}}}"
  ),
  list(
    "equal",
    quote({isTRUE(. == .ref)}),
    "{{.}} is not equal to {{{.ref$value}}}"
  ),
  list(
    "not_equal",
    quote({isTRUE(. != .ref)}),
    "{{.}} equals {{{.ref$value}}}"
  ),
  list(
    "all_equal",
    function(., .ref, ...)
      isTRUE(all.equal(., .ref, ...)),
    "{{.}} is not (all) equal to {{{.ref$expr}}}"
  ),
  list(
    "not_all_equal",
    function(., .ref, ...)
      rlang::is_false(all.equal(., .ref, ...)),
    "{{.}} is (all) equal to {{{.ref$expr}}}"
  ),
  list(
    "gt",
    quote({isTRUE(. > .ref)}),
    "{{.}} is not greater than {{{.ref$value}}}"
  ),
  list(
    "lt",
    quote({isTRUE(. < .ref)}),
    "{{.}} is not less than {{{.ref$value}}}"
  ),
  list(
    "gte",
    quote({isTRUE(. >= .ref)}),
    "{{.}} is not greater than or equal to {{{.ref$value}}}"
  ),
  list(
    "lte",
    quote({isTRUE(. <= .ref)}),
    "{{.}} is not less than or equal to {{{.ref$value}}}"
  )
)
comparisons$sets <- list(
  list(
    "in",
    quote({isTRUE(. %in% .ref)}),
    "{{.}} is not in {{{.ref$expr}}}"
  ),
  list(
    "not_in",
    quote({isTRUE(! . %in% .ref)}),
    "{{.}} is in {{{.ref$expr}}}"
  ),
  list(
    "contains",
    quote({all(.ref %in% .)}),
    "{{.}} does not contain {{{.ref$expr}}}"
  ),
  list(
    "doesnt_contain",
    quote({any(! .ref %in% .)}),
    "{{.}} contains {{{.ref$expr}}}"
  ),
  list(
    "contained_in",
    quote({all(. %in% .ref)}),
    "{{.}} is not contained in {{{.ref$expr}}}"
  ),
  list(
    "not_contained_in",
    quote({any(! . %in% .ref)}),
    "{{.}} is contained in {{{.ref$expr}}}"
  ),
  list(
    "intersects",
    quote({length(intersect(., .ref)) > 0}),
    "{{.}} does not intersect {{{.ref$expr}}}"
  ),
  list(
    "doesnt_intersect",
    quote({length(intersect(., .ref)) == 0}),
    "{{.}} intersects {{{.ref$expr}}}"
  ),
  list(
    "setequal",
    setequal,
    "{{.}} and {{{.ref$expr}}} are not equal as sets"
  )
)

predicates <- list(
  boolean     = NULL,
  type        = NULL,
  scalar_type = NULL,
  object      = NULL,
  misc        = NULL
)
predicates$boolean <- list(
  list(
    "true",
    isTRUE,
    "{{.}} is not true"
  ),
  list(
    "is",
    isTRUE,
    "{{.}} is not true"
  ),
  list(
    "false",
    quote({identical(., FALSE)}),
    "{{.}} is not false"
  ),
  list(
    "not",
    quote({identical(., FALSE)}),
    "{{.}} is not false"
  ),
  list(
    "all",
    all,
    "{{.}} not all true"
  ),
  list(
    "none",
    quote({all(! .)}),
    "{{.}} not all false"
  ),
  list(
    "any",
    any,
    "{{.}} all false"
  )
)

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


assign_with <- function(f) {
  function(prefix, xs) {
    env <- parent.frame()
    for (x in xs) {
      nm <- paste0(prefix, x[[1]])
      assign(nm, f(UQ(x[[2]]), x[[3]]), envir = env)
    }
  }
}
assign_lcl_comparisons <- assign_with(localize_comparison)
assign_lcl_predicates  <- assign_with(localize)

#' @rawNamespace exportPattern("^v_.*$")
assign_lcl_comparisons("vld_", unlist(comparisons, recursive = FALSE))
