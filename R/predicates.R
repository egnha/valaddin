predicates <- list(
  boolean  = NULL,
  object   = NULL,
  pattern  = NULL,
  property = NULL,
  relation = NULL,
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
    function(., na.rm = FALSE) all(!., na.rm = na.rm)
  ),
  list(
    "all_map",
    "{{.}} is not all true when mapped by {{.expr$f}}",
    function(., f, na.rm = FALSE)
      all(vapply(., f, logical(1)), na.rm = na.rm),
    transformer = list(f = as_function)
  ),
  list(
    "any_map",
    "{{.}} is all false when mapped by {{.expr$f}}",
    function(., f, na.rm = FALSE)
      any(vapply(., f, logical(1)), na.rm = na.rm),
    transformer = list(f = as_function)
  ),
  list(
    "none_map",
    "{{.}} not all false when mapped by {{.expr$f}}",
    function(., f, na.rm = FALSE)
      all(!vapply(., f, logical(1)), na.rm = na.rm),
    transformer = list(f = as_function)
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
    function(.) !is_na(.)
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
    is_named
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
        paste("{{.}} is not", this),
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
  atomic     = "an atomic vector{{of_length(.value$n)}}",
  list       = "a list{{of_length(.value$n)}}",
  vector     = "an atomic vector or list{{of_length(.value$n)}}",
  logical    = "a logical vector{{of_length(.value$n)}}",
  integer    = "an integer vector{{of_length(.value$n)}}",
  integerish = "an integerish vector{{of_length(.value$n)}}",
  double     = "a double vector{{of_length(.value$n)}}",
  character  = "a character vector{{of_length(.value$n)}}",
  raw        = "a raw vector{{of_length(.value$n)}}"
)
of_length <- function(n) {
  if (is.null(n)) "" else paste(" of length", n)
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
        if (! typeof(.) %in% c("double", "integer"))
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
        if (typeof(.) != "complex")
          return(FALSE)
        if (!is.null(n) && length(.) != n)
          return(FALSE)
        TRUE
      }
    ),
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
  ),
  make_predicate_data("base", types_base, "is."),
  make_predicate_data("rlang", types_rlang, "is_")
)

for (x in unlist(predicates, recursive = FALSE)) {
  nm <- paste0("vld_", x[[1]])
  transformer <- x$transformer %||% list()
  assign(nm, checker(UQ(x[[2]]) := UQ(x[[3]]), UQS(transformer)))
}

#' @rawNamespace exportPattern("^vld_.+$")
NULL

# Documentation -----------------------------------------------------------

nms_checkers <- lapply(predicates, function(x) {
  nms <- vapply(x, `[[`, character(1), 1)
  paste0("vld_", nms)
})
# Order types as they appear in the "R Language Definition" manual
nms_checkers$type <- c(
  "vld_null",
  "vld_symbol",
  "vld_pairlist",
  "vld_closure",
  "vld_environment",
  "vld_language",
  "vld_atomic",
  "vld_vector",
  "vld_logical",
  "vld_boolean",
  "vld_numerical",
  "vld_number",
  "vld_integer",
  "vld_integerish",
  "vld_double",
  "vld_complex",
  "vld_character",
  "vld_string",
  "vld_list",
  "vld_raw"
)

#' Boolean checkers
#'
#' @evalRd rd_alias(nms_checkers$boolean)
#' @evalRd rd_usage(nms_checkers$boolean)
#'
#' @param \dots Expressions to validate.
#' @param f Function to map over the expressions to validate.
#' @param na.rm Should `NA` values be disregarded?
#'
#' @examples
#' f <- function(x, y) NULL
#'
#' ## Require x, y to have the same length
#' foo <- firmly(f, vld_true(length(x) == length(y)))
#' foo(runif(3), rnorm(3))
#' \dontrun{
#' foo(runif(2), rnorm(3))}
#'
#' ## Require x to contain only non-empty objects
#' error_msg <- "{{.}} contains empty objects"
#' bar <- firmly(f, error_msg := vld_all_map(~ length(.) != 0, x))
#' bar(1:2)
#' \dontrun{
#' bar(list(1, NULL))}
#'
#' ## Or more efficiently, in a vectorized manner:
#' baz <- firmly(f, vld_all("x contains empty objects" := lengths(x) != 0))
#' baz(1:2)
#' \dontrun{
#' baz(list(1, NULL))}
#'
#' @name checker-boolean
NULL

#' Object checkers
#'
#' @evalRd rd_alias(nms_checkers$object)
#' @evalRd rd_usage(nms_checkers$object)
#'
#' @param \dots Expressions to validate.
#'
#' @examples
#' row_sums <- firmly(rowSums, vld_matrix(x))
#' row_sums(matrix(1:6, 2, 3))
#' \dontrun{
#' row_sums(mtcars)}
#'
#' @name checker-object
NULL

#' Pattern checkers
#'
#' @evalRd rd_alias(nms_checkers$pattern)
#' @evalRd rd_usage(nms_checkers$pattern)
#'
#' @param \dots Expressions to validate.
#' @param pattern Regular expression.
#' @param ignore.case Should pattern matching ignore case?
#' @param perl Should Perl-compatible regular expressions be used?
#'
#' @seealso [grepl()]
#'
#' @examples
#' ymd <- function(y, m, d) paste(y, m, d, sep = "/")
#' too_old <- "Not a 21st-century year"
#' recent_ymd <-
#'   firmly(ymd, too_old := vld_grepl("^20[[:digit:]]{2}$", as.character(y)))
#' recent_ymd(2017, 01, 01)
#' \dontrun{
#' recent_ymd(1999, 01, 01)}
#'
#' @name checker-pattern
NULL

#' Property checkers
#'
#' @evalRd rd_alias(nms_checkers$property)
#' @evalRd rd_usage(nms_checkers$property)
#'
#' @param \dots Expressions to validate.
#' @param nm,nms Name(s).
#' @param n Length of vector or list.
#' @param which Object attribute(s).
#' @param what Class name.
#'
#' @name checker-property
NULL

#' Relation checkers
#'
#' @evalRd rd_alias(nms_checkers$relation)
#' @evalRd rd_usage(nms_checkers$relation)
#'
#' @param \dots Expressions to validate.
#' @param to Object to match.
#' @param lwr,upr Lower/upper bound.
#' @param na.rm Should `NA` values be disregarded?
#'
#' @seealso [all.equal()], [identical()]
#'
#' @name checker-relation
NULL

#' Scalar type checkers
#'
#' @evalRd rd_alias(nms_checkers$scalar)
#' @evalRd rd_usage(nms_checkers$scalar)
#'
#' @param \dots Expressions to validate.
#'
#' @seealso [vld_singleton()]
#'
#' @name checker-scalar-type
NULL

#' Set comparison checkers
#'
#' @evalRd rd_alias(nms_checkers$sets)
#' @evalRd rd_usage(nms_checkers$sets)
#'
#' @param \dots Expressions to validate
#' @param set Vector to compare.
#'
#' @name checker-sets
NULL

#' Type checkers
#'
#' @evalRd rd_alias(nms_checkers$type)
#' @evalRd rd_usage(nms_checkers$type)
#'
#' @param \dots Expressions to validate.
#' @param n Length of vector.
#' @param encoding Encoding of a string or character vector. One of `UTF-8`,
#'   `latin1`, or `unknown`.
#'
#' @seealso [Type predicates][rlang::type-predicates]
#'
#' @examples
#' f <- function(x, y, z) NULL
#'
#' ## Require all arguments to be integer (vectors)
#' foo <- firmly(f, vld_integer())
#' foo(0L, 1:2, length(letters))
#' \dontrun{
#' foo(0L, c(1, 2), length(letters))}
#'
#' ## Require all arguments to be scalar integers
#' bar <- firmly(f, vld_integer(n = 1))
#' bar(0L, 1L, length(NA))
#' \dontrun{
#' bar(0L, 1L, lengths(letters))}
#'
#' ## Require x, y to be character (vectors), and z to be an length-1 list
#' baz <- firmly(f, vld_character(x, y), vld_list(n = 1, z))
#' baz(letters, "text", list(1))
#' \dontrun{
#' baz(0, "text", list(1, 2))}
#'
#' @name checker-type
NULL
