predicates <- list(
  boolean  = NULL,
  object   = NULL,
  pattern  = NULL,
  property = NULL,
  relation = NULL,
  scalar   = NULL,
  sets     = NULL,
  type     = NULL
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
    function(.) identical(., FALSE),
    "{{.}} is not false"
  ),
  list(
    "not",
    function(.) identical(., FALSE),
    "{{.}} is not false"
  ),
  list(
    "all",
    function(., na.rm = FALSE) all(., na.rm = na.rm),
    "{{.}} is not all true"
  ),
  list(
    "any",
    function(., na.rm = FALSE) any(., na.rm = na.rm),
    "{{.}} is all false"
  ),
  list(
    "none",
    function(., na.rm = FALSE) all(! ., na.rm = na.rm),
    "{{.}} not all false"
  ),
  list(
    "all_map",
    function(., f, na.rm = FALSE)
      all(vapply(., rlang::as_function(f), logical(1)), na.rm = na.rm),
    "{{.}} is not all true when mapped by {{.expr$f}}"
  ),
  list(
    "any_map",
    function(., f, na.rm = FALSE)
      any(vapply(., rlang::as_function(f), logical(1)), na.rm = na.rm),
    "{{.}} is all false when mapped by {{.expr$f}}"
  )
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
    function(.) inherits(., "formula"),
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
predicates$pattern <- list(
  list(
    "grepl",
    function(., pattern, ignore.case = FALSE, perl = FALSE)
      all(grepl(pattern, ., ignore.case = ignore.case, perl = perl)),
    "Pattern {{.expr$pattern}} is not matched in {{.}}"
  )
)
predicates$property <- list(
  list(
    "not_null",
    function(.) !is.null(.),
    "{{.}} is NULL"
  ),
  list(
    "not_empty",
    function(.) length(.) != 0,
    "{{.}} is empty"
  ),
  list(
    "singleton",
    function(.) length(.) == 1,
    "{{.}} is not a singleton"
  ),
  list(
    "not_na",
    function(.) !is.na(.),
    "{{.}} is NA"
  ),
  list(
    "without_na",
    function(.) !anyNA(.),
    "{{.}} has an NA"
  ),
  list(
    "sorted",
    function(.) !is.unsorted(.),
    "{{.}} is not sorted"
  ),
  list(
    "named",
    rlang::is_named,
    "{{.}} is not named"
  ),
  list(
    "has_name",
    function(., nm) isTRUE(nm %in% names(.)),
    "{{.}} does not have name {{.value$nm}}"
  ),
  list(
    "has_names",
    function(., nms) all(nms %in% names(.)),
    "{{.expr$nms}} are not all names of {{.}}"
  ),
  list(
    "has_length",
    function(., n) length(.) == n,
    "{{.}} is not of length {{.value$n}}"
  ),
  list(
    "has_attr",
    function(., which) !is.null(attr(., which, exact = TRUE)),
    "{{.}} does not have attribute {{.value$which}}"
  ),
  list(
    "has_attrs",
    function(., which) all(which %in% names(attributes(.))),
    "{{.expr$which}} are not all attributes of {{.}}"
  ),
  list(
    "inherits",
    inherits,
    '{{.}} is not of class "{{.value$what}}"'
  )
)
predicates$relation <- list(
  list(
    "identical",
    function(., y) identical(., y),
    "{{.}} is not identical to {{.expr$y}}"
  ),
  list(
    "not_identical",
    function(., y) !identical(., y),
    "{{.}} is identical to {{.expr$y}}"
  ),
  list(
    "equal",
    function(., y) isTRUE(. == y),
    "{{.}} is not equal to {{.expr$y}}"
  ),
  list(
    "not_equal",
    function(., y) isTRUE(. != y),
    "{{.}} equals {{.expr$y}}"
  ),
  list(
    "all_equal",
    function(., target, ...) isTRUE(all.equal(target, ., ...)),
    "{{.}} is not (all) equal to {{.expr$target}}"
  ),
  list(
    "not_all_equal",
    function(., target, ...) rlang::is_false(all.equal(target, ., ...)),
    "{{.}} is (all) equal to {{.expr$target}}"
  ),
  list(
    "gt",
    function(., lwr) isTRUE(. > lwr),
    "{{.}} is not greater than {{.value$lwr}}"
  ),
  list(
    "lt",
    function(., upr) isTRUE(. < upr),
    "{{.}} is not less than {{.value$upr}}"
  ),
  list(
    "gte",
    function(., lwr) isTRUE(. >= lwr),
    "{{.}} is not greater than or equal to {{.value$lwr}}"
  ),
  list(
    "lte",
    function(., upr) isTRUE(. <= upr),
    "{{.}} is not less than or equal to {{.value$upr}}"
  )
)
predicates$scalar <- list(
  list(
    "number",
    function(.)
      typeof(.) %in% c("double", "integer") && length(.) == 1 && !is.na(.),
    "{{.}} is not a number"
  ),
  list(
    "whole_number",
    function(.)
      is.integer(.) && length(.) == 1 && !is.na(.),
    "{{.}} is not a whole number"
  ),
  list(
    "boolean",
    function(.)
      is.logical(.) && length(.) == 1 && !is.na(.),
    "{{.}} is not a boolean"
  ),
  list(
    "string",
    function(.)
      is.character(.) && length(.) == 1 && !is.na(.),
    "{{.}} is not a string"
  )
)
predicates$sets <- list(
  list(
    "in",
    function(., set) isTRUE(. %in% set),
    "{{.}} is not in {{.expr$set}}"
  ),
  list(
    "not_in",
    function(., set) isTRUE(! . %in% set),
    "{{.}} is in {{.expr$set}}"
  ),
  list(
    "contains",
    function(., set) all(set %in% .),
    "{{.}} does not contain {{.expr$set}}"
  ),
  list(
    "doesnt_contain",
    function(., set) any(! set %in% .),
    "{{.}} contains {{.expr$set}}"
  ),
  list(
    "contained_in",
    function(., set) all(. %in% set),
    "{{.}} is not contained in {{.expr$set}}"
  ),
  list(
    "not_contained_in",
    function(., set) any(! . %in% set),
    "{{.}} is contained in {{.expr$set}}"
  ),
  list(
    "intersects",
    function(., set) length(intersect(., set)) != 0,
    "{{.}} does not intersect {{.expr$set}}"
  ),
  list(
    "doesnt_intersect",
    function(., set) length(intersect(., set)) == 0,
    "{{.}} intersects {{.expr$set}}"
  ),
  list(
    "setequal",
    function(., set) setequal(., set),
    "{{.}} and {{.expr$set}} are not equal as sets"
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
types_base <- list(
  null        = "NULL",
  symbol      = "a symbol",
  pairlist    = "a pairlist",
  environment = "an environment"
)
types_rlang <- list(
  atomic      = "an atomic vector{{of_length(.value$n)}}",
  list        = "a list{{of_length(.value$n)}}",
  vector      = "an atomic vector or list{{of_length(.value$n)}}",
  logical     = "a logical vector{{of_length(.value$n)}}",
  integer     = "an integer vector{{of_length(.value$n)}}",
  double      = "a double vector{{of_length(.value$n)}}",
  character   = "a character vector{{of_length(.value$n)}}",
  raw         = "a raw vector{{of_length(.value$n)}}"
)
of_length <- function(n) {
  if (is.null(n)) "" else sprintf(" of length %s", n)
}
predicates$type <- c(
  list(
    list(
      "closure",
      function(.) typeof(.) == "closure",
      "{{.}} is not a closure"
    ),
    list(
      "numerical",
      function(., n = NULL) {
        if (!typeof(.) %in% c("double", "integer"))
          return(FALSE)
        if (!is.null(n) && length(.) != n)
          return(FALSE)
        TRUE
      },
      "{{.}} is not a numerical vector{{of_length(.value$n)}}"
    )
  ),
  make_predicate_data("base", types_base, "is."),
  make_predicate_data("rlang", types_rlang, "is_")
)

for (x in unlist(predicates, recursive = FALSE)) {
  nm <- paste0("vld_", x[[1]])
  assign(nm, localize(UQ(x[[3]]) := UQ(x[[2]])))
}

#' @rawNamespace exportPattern("^vld_.*$")
NULL

# Documentation -----------------------------------------------------------

nms_predicates <- lapply(predicates, function(x)
  paste0("vld_", vapply(x, `[[`, character(1), 1))
)

#' Boolean predicates
#'
#' @evalRd rd_alias(nms_predicates$boolean)
#' @evalRd rd_usage(nms_predicates$boolean)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-boolean
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

#' Pattern predicates
#'
#' @evalRd rd_alias(nms_predicates$pattern)
#' @evalRd rd_usage(nms_predicates$pattern)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-pattern
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

#' Relation predicates
#'
#' @evalRd rd_alias(nms_predicates$relation)
#' @evalRd rd_usage(nms_predicates$relation)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-relation
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

#' Set comparison predicates
#'
#' @evalRd rd_alias(nms_predicates$sets)
#' @evalRd rd_usage(nms_predicates$sets)
#'
#' @param \dots Expressions to validate
#'
#' @name predicates-sets
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
