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
    '{{.}} does not have name "{{{.ref$value}}}"'
  ),
  list(
    "has_names",
    quote({all(.ref %in% names(.))}),
    "{{{comma_collapse(.ref$value)}}} are not all names of {{.}}"
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
    "{{{comma_collapse(.ref$value)}}} are not all attributes of {{.}}"
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
  boolean  = NULL,
  type     = NULL,
  scalar   = NULL,
  object   = NULL,
  property = NULL
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
make_predicate_data <- function(ns, xs, prefix) {
  unname(
    Map(function(nm, this) {
      list(
        nm,
        getExportedValue(ns, paste0(prefix, nm)),
        sprintf("{{.}} is not %s", this)
      )
    }, names(xs), xs)
  )
}
types <- list(
  null        = "NULL",
  symbol      = "a symbol",
  pairlist    = "a pairlist",
  environment = "an environment",
  language    = "a language object",
  list        = "a list",
  logical     = "a logical vector",
  integer     = "an integer vector",
  double      = "a double vector",
  complex     = "a complex vector",
  character   = "a character vector",
  raw         = "a raw vector"
)
predicates$type <- c(
  list(
    list(
      "closure",
      quote({typeof(.) == "closure"}),
      "{{.}} is not a closure"
    ),
    list(
      "numerical",
      quote({typeof(.) %in% c("double", "integer")}),
      "{{.}} is not a double/integer vector"
    )
  ),
  make_predicate_data("base", types, "is.")
)
make_scalar_type_data <- function(xs, prefix) {
  unname(
    Map(function(nm, this) {
      list(
        paste0("scalar_", nm),
        bquote({.(as.name(paste0(prefix, nm)))(.) && length(.) == 1}),
        sprintf("{{.}} is not a single-element %s", this)
      )
    }, names(xs), xs)
  )
}
scalar <- list(
  atomic    = "atomic vector",
  list      = "list",
  logical   = "logical vector",
  integer   = "integer vector",
  double    = "double vector",
  complex   = "complex vector",
  character = "character vector",
  raw       = "raw vector",
  vector    = "vector"
)
is_scalar_numerical <- function(.) {
  typeof(.) %in% c("double", "integer") && length(.) == 1
}
predicates$scalar <- c(
  list(
    list(
      "scalar_numerical",
      is_scalar_numerical,
      "{{.}} is not a single-element double/integer vector"
    ),
    list(
      "number",
      bquote({
        typeof(.) %in% c("double", "integer") && length(.) == 1 && !is.na(.)
      }),
      "{{.}} is not a number"
    ),
    list(
      "whole_number",
      quote({is.integer(.) && length(.) == 1 && !is.na(.)}),
      "{{.}} is not a whole number"
    ),
    list(
      "boolean",
      quote({is.logical(.) && length(.) == 1 && !is.na(.)}),
      "{{.}} is not a boolean"
    ),
    list(
      "string",
      quote({is.character(.) && length(.) == 1 && !is.na(.)}),
      "{{.}} is not a string"
    )
  ),
  make_scalar_type_data(scalar, "is.")
)
predicates$object <- list(
  list(
    "call",
    is.call,
    "{{.}} is not a call"
  ),
  list(
    "factor",
    is.factor,
    "{{.}} is not a factor"
  ),
  list(
    "data_frame",
    is.data.frame,
    "{{.}} is not a data frame"
  ),
  list(
    "matrix",
    is.matrix,
    "{{.}} is not a matrix"
  ),
  list(
    "formula",
    quote({inherits(., "formula")}),
    "{{.}} is not a formula"
  ),
  list(
    "function",
    is.function,
    "{{.}} is not a function"
  ),
  list(
    "quosure",
    rlang::is_quosure,
    "{{.}} is not a quosure"
  ),
  list(
    "quosures",
    rlang::is_quosures,
    "{{.}} is not a list of quosures"
  )
)
predicates$property <- list(
  list(
    "not_null",
    quote({!is.null(.)}),
    "{{.}} is NULL"
  ),
  list(
    "not_empty",
    quote({length(.) != 0}),
    "{{.}} is empty"
  ),
  list(
    "singleton",
    quote({length(.) == 1}),
    "{{.}} is not a length-one object"
  ),
  list(
    "not_na",
    quote({!is.na(.)}),
    "{{.}} is NA"
  ),
  list(
    "has_no_na",
    quote({!anyNA(.)}),
    "{{.}} has an NA"
  ),
  list(
    "sorted",
    quote({!is.unsorted(.)}),
    "{{.}} is not sorted"
  ),
  list(
    "recursive",
    is.recursive,
    "{{.}} is not recursive"
  ),
  list(
    "named",
    rlang::is_named,
    "{{.}} is not named"
  )
)

for (x in unlist(comparisons, recursive = FALSE)) {
  nm <- paste0("vld_", x[[1]])
  assign(nm, localize_comparison(UQ(x[[2]]), x[[3]]))
}
for (x in unlist(predicates, recursive = FALSE)) {
  nm <- paste0("vld_", x[[1]])
  assign(nm, localize(UQ(x[[2]]), x[[3]]))
}

#' @rawNamespace exportPattern("^vld_.*$")
NULL

# Documentation -----------------------------------------------------------

names_predicates <- function(preds) {
  lapply(preds, function(x)
    paste0("vld_", vapply(x, `[[`, character(1), 1))
  )
}
nms_predicates  <- names_predicates(predicates)
nms_comparisons <- names_predicates(comparisons)

#' Boolean comparison predicates
#'
#' @evalRd rd_alias(nms_comparisons$boolean)
#' @evalRd rd_usage(nms_comparisons$boolean)
#'
#' @param \dots Expressions to validate
#' @param .ref Comparison object.
#'
#' @name comparisons-boolean
NULL

#' Pattern comparison predicates
#'
#' @evalRd rd_alias(nms_comparisons$pattern)
#' @evalRd rd_usage(nms_comparisons$pattern)
#'
#' @param \dots Expressions to validate
#' @param .ref Comparison object.
#'
#' @name comparisons-pattern
NULL

#' Property comparison predicates
#'
#' @evalRd rd_alias(nms_comparisons$property)
#' @evalRd rd_usage(nms_comparisons$property)
#'
#' @param \dots Expressions to validate
#' @param .ref Comparison object.
#'
#' @name comparisons-property
NULL

#' Relation predicates
#'
#' @evalRd rd_alias(nms_comparisons$relation)
#' @evalRd rd_usage(nms_comparisons$relation)
#'
#' @param \dots Expressions to validate
#' @param .ref Comparison object.
#'
#' @name comparisons-relation
NULL

#' Set comparison predicates
#'
#' @evalRd rd_alias(nms_comparisons$sets)
#' @evalRd rd_usage(nms_comparisons$sets)
#'
#' @param \dots Expressions to validate
#' @param .ref Comparison object.
#'
#' @name comparisons-sets
NULL

#' Boolean predicates
#'
#' @evalRd rd_alias(nms_predicates$boolean)
#' @evalRd rd_usage(nms_predicates$boolean)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-boolean
NULL

#' Type predicates
#'
#' @evalRd rd_alias(nms_predicates$type)
#' @evalRd rd_usage(nms_predicates$type)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-type
NULL

#' Scalar type predicates
#'
#' @evalRd rd_alias(nms_predicates$scalar)
#' @evalRd rd_usage(nms_predicates$scalar)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-scalar-type
NULL

#' Object predicates
#'
#' @evalRd rd_alias(nms_predicates$object)
#' @evalRd rd_usage(nms_predicates$object)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-object
NULL

#' Property predicates
#'
#' @evalRd rd_alias(nms_predicates$property)
#' @evalRd rd_usage(nms_predicates$property)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-property
NULL
