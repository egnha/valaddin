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
    "{{.}} is not true",
    isTRUE
  ),
  list(
    "is",
    "{{.}} is not true",
    isTRUE
  ),
  list(
    "false",
    "{{.}} is not false",
    function(.) identical(., FALSE)
  ),
  list(
    "not",
    "{{.}} is not false",
    function(.) identical(., FALSE)
  ),
  list(
    "all",
    "{{.}} is not all true",
    function(., na.rm = FALSE) all(., na.rm = na.rm)
  ),
  list(
    "any",
    "{{.}} is all false",
    function(., na.rm = FALSE) any(., na.rm = na.rm)
  ),
  list(
    "none",
    "{{.}} not all false",
    function(., na.rm = FALSE) all(! ., na.rm = na.rm)
  ),
  list(
    "all_map",
    "{{.}} is not all true when mapped by {{.expr$f}}",
    function(., f, na.rm = FALSE)
      all(vapply(., rlang::as_function(f), logical(1)), na.rm = na.rm)
  ),
  list(
    "any_map",
    "{{.}} is all false when mapped by {{.expr$f}}",
    function(., f, na.rm = FALSE)
      any(vapply(., rlang::as_function(f), logical(1)), na.rm = na.rm)
  ),
  list(
    "none_map",
    "{{.}} not all false when mapped by {{.expr$f}}",
    function(., f, na.rm = FALSE)
      all(! vapply(., rlang::as_function(f), logical(1)), na.rm = na.rm)
  )
)
predicates$object <- list(
  list(
    "call",
    "{{.}} is not a call",
    is.call
  ),
  list(
    "factor",
    "{{.}} is not a factor",
    is.factor
  ),
  list(
    "data_frame",
    "{{.}} is not a data frame",
    is.data.frame
  ),
  list(
    "matrix",
    "{{.}} is not a matrix",
    is.matrix
  ),
  list(
    "formula",
    "{{.}} is not a formula",
    function(.) inherits(., "formula")
  ),
  list(
    "function",
    "{{.}} is not a function",
    is.function
  )
)
predicates$pattern <- list(
  list(
    "grepl",
    "Pattern {{.expr$pattern}} is not matched in {{.}}",
    function(., pattern, ignore.case = FALSE, perl = FALSE)
      all(grepl(pattern, ., ignore.case = ignore.case, perl = perl))
  )
)
predicates$property <- list(
  list(
    "not_null",
    "{{.}} is NULL",
    function(.) !is.null(.)
  ),
  list(
    "not_empty",
    "{{.}} is empty",
    function(.) length(.) != 0
  ),
  list(
    "singleton",
    "{{.}} is not a singleton",
    function(.) length(.) == 1
  ),
  list(
    "not_na",
    "{{.}} is NA",
    function(.) !rlang::is_na(.)
  ),
  list(
    "without_na",
    "{{.}} has an NA",
    function(.) !anyNA(.)
  ),
  list(
    "sorted",
    "{{.}} is not sorted",
    function(.) !is.unsorted(.)
  ),
  list(
    "named",
    "{{.}} is not named",
    rlang::is_named
  ),
  list(
    "has_name",
    "{{.}} does not have name {{.value$nm}}",
    function(., nm) isTRUE(nm %in% names(.))
  ),
  list(
    "has_names",
    "{{.expr$nms}} are not all names of {{.}}",
    function(., nms) all(nms %in% names(.))
  ),
  list(
    "has_length",
    "{{.}} is not of length {{.value$n}}",
    function(., n) length(.) == n
  ),
  list(
    "has_attr",
    "{{.}} does not have attribute {{.value$which}}",
    function(., which) !is.null(attr(., which, exact = TRUE))
  ),
  list(
    "has_attrs",
    "{{.expr$which}} are not all attributes of {{.}}",
    function(., which) all(which %in% names(attributes(.)))
  ),
  list(
    "inherits",
    '{{.}} is not of class "{{.value$what}}"',
    function(., what) inherits(., what)
  )
)
predicates$relation <- list(
  list(
    "identical",
    "{{.}} is not identical to {{.expr$to}}",
    function(., to) identical(., to)
  ),
  list(
    "not_identical",
    "{{.}} is identical to {{.expr$to}}",
    function(., to) !identical(., to)
  ),
  list(
    "equal",
    "{{.}} does not equal {{.expr$to}}",
    function(., to) isTRUE(all.equal(to, .))
  ),
  list(
    "not_equal",
    "{{.}} equals {{.expr$to}}",
    function(., to) !isTRUE(all.equal(to, .))
  ),
  list(
    "equivalent",
    "{{.}} is not equivalent to {{.expr$to}}",
    function(., to) isTRUE(all.equal(to, ., check.attributes = FALSE))
  ),
  list(
    "not_equivalent",
    "{{.}} is equivalent to {{.expr$to}}",
    function(., to) !isTRUE(all.equal(to, ., check.attributes = FALSE))
  ),
  list(
    "gt",
    "{{.}} is not greater than {{.value$lwr}}",
    function(., lwr, na.rm = FALSE) all(. > lwr, na.rm = na.rm)
  ),
  list(
    "lt",
    "{{.}} is not less than {{.value$upr}}",
    function(., upr, na.rm = FALSE) all(. < upr, na.rm = na.rm)
  ),
  list(
    "gte",
    "{{.}} is not greater than or equal to {{.value$lwr}}",
    function(., lwr, na.rm = FALSE) all(. >= lwr, na.rm = na.rm)
  ),
  list(
    "lte",
    "{{.}} is not less than or equal to {{.value$upr}}",
    function(., upr, na.rm = FALSE) all(. <= upr, na.rm = na.rm)
  )
)
predicates$scalar <- list(
  list(
    "number",
    "{{.}} is not a number",
    function(.)
      typeof(.) %in% c("double", "integer") && length(.) == 1 && !is.na(.)
  ),
  list(
    "boolean",
    "{{.}} is not a boolean",
    function(.)
      is.logical(.) && length(.) == 1 && !is.na(.)
  ),
  list(
    "string",
    "{{.}} is not a string",
    function(.)
      is.character(.) && length(.) == 1 && !is.na(.)
  )
)
predicates$sets <- list(
  list(
    "in",
    "{{.}} is not in {{.expr$set}}",
    function(., set) isTRUE(. %in% set)
  ),
  list(
    "not_in",
    "{{.}} is in {{.expr$set}}",
    function(., set) isTRUE(! . %in% set)
  ),
  list(
    "include",
    "{{.}} does not include {{.expr$set}}",
    function(., set) all(set %in% .)
  ),
  list(
    "exclude",
    "{{.}} intersects {{.expr$set}}",
    function(., set) all(! set %in% .)
  ),
  list(
    "within",
    "{{.}} is not contained in {{.expr$set}}",
    function(., set) all(. %in% set)
  ),
  list(
    "intersect",
    "{{.}} is disjoint from {{.expr$set}}",
    function(., set) length(intersect(., set)) != 0
  ),
  list(
    "avoid",
    "{{.}} intersects {{.expr$set}}",
    function(., set) length(intersect(., set)) == 0
  ),
  list(
    "setequal",
    "{{.}} and {{.expr$set}} are not equal as sets",
    function(., set) setequal(., set)
  )
)
make_predicate_data <- function(ns, xs, prefix) {
  unname(
    Map(function(nm, this) {
      list(
        nm,
        sprintf("{{.}} is not %s", this),
        getExportedValue(ns, paste0(prefix, nm))
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
      "{{.}} is not a closure",
      function(.) typeof(.) == "closure"
    ),
    list(
      "language",
      "{{.}} is not of type 'language'",
      function(.) typeof(.) == "language"
    ),
    list(
      "numerical",
      "{{.}} is not a numerical vector{{of_length(.value$n)}}",
      function(., n = NULL) {
        if (!typeof(.) %in% c("double", "integer"))
          return(FALSE)
        if (!is.null(n) && length(.) != n)
          return(FALSE)
        TRUE
      }
    ),
    list(
      "complex",
      "{{.}} is not a complex vector{{of_length(.value$n)}}",
      function(., n = NULL) {
        if (!typeof(.) == "complex")
          return(FALSE)
        if (!is.null(n) && length(.) != n)
          return(FALSE)
        TRUE
      }
    )
  ),
  make_predicate_data("base", types_base, "is."),
  make_predicate_data("rlang", types_rlang, "is_")
)

for (x in unlist(predicates, recursive = FALSE)) {
  nm <- paste0("vld_", x[[1]])
  assign(nm, checker(UQ(x[[2]]) := UQ(x[[3]])))
}

#' @rawNamespace exportPattern("^vld_.+$")
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
