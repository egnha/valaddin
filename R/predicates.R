#' Basic predicates for validation
#'
#' @description The following predicate functions augment predicates from the
#'   \pkg{base} and \pkg{rlang} packages to enable them to produce informative
#'   error messages when used as checks in [fasten()], [firmly()], [validify()],
#'   [validate()].
#'
#' - [Boolean predicates][predicates-boolean]
#' - [Object predicates][predicates-object]
#' - [Pattern predicates][predicates-pattern]
#' - [Property predicates][predicates-property]
#' - [Relational predicates][predicates-relational]
#' - [Set predicates][predicates-set]
#' - [Type predicates][predicates-type]
#'
#' @examples
#' ## vld_double() and rlang::is_double() are identical as functions
#' vld_double(runif(2))
#' vld_double(runif(2), n = 1)
#' vld_double(1:2)
#' rlang::is_double(runif(2))
#' rlang::is_double(runif(2), n = 1)
#' rlang::is_double(1:2)
#'
#' ## But when rlang::is_double() is used in firmly(),
#' ## it produces an auto-generated error message ...
#' \dontrun{
#' firmly(function(x) x, rlang::is_double(n = 1))(runif(2))}
#'
#' ## ... whereas vld_double() produces a specialized error message
#' \dontrun{
#' firmly(function(x) x, vld_double(n = 1))(runif(2))}
#'
#' \dontrun{
#' validate(mtcars, is.matrix, {"cylinder" %in% names(.)})
#' validate(mtcars, vld_matrix, vld_has_name("cylinder"))}
#'
#' @name predicates
NULL

predicates <- list(
  boolean  = NULL,
  object   = NULL,
  pattern  = NULL,
  property = NULL,
  relation = NULL,
  set      = NULL,
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
    function(x) identical(x, FALSE)
  ),
  list(
    "not",
    "{{.}} is not false",
    function(x) identical(x, FALSE)
  ),
  list(
    "all",
    "{{.}} is not all true",
    function(x, na.rm = FALSE) all(x, na.rm = na.rm)
  ),
  list(
    "any",
    "{{.}} is all false",
    function(x, na.rm = FALSE) any(x, na.rm = na.rm)
  ),
  list(
    "none",
    "{{.}} not all false",
    function(x, na.rm = FALSE) all(!x, na.rm = na.rm)
  ),
  list(
    "all_map",
    "{{.}} is not all true when mapped by {{.expr$f}}",
    function(x, f, na.rm = FALSE)
      all(vapply(x, f, logical(1)), na.rm = na.rm)
  ),
  list(
    "any_map",
    "{{.}} is all false when mapped by {{.expr$f}}",
    function(x, f, na.rm = FALSE)
      any(vapply(x, f, logical(1)), na.rm = na.rm)
  ),
  list(
    "none_map",
    "{{.}} not all false when mapped by {{.expr$f}}",
    function(x, f, na.rm = FALSE)
      all(!vapply(x, f, logical(1)), na.rm = na.rm)
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
    function(x) inherits(x, "formula")
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
    function(x, pattern, ignore.case = FALSE, perl = FALSE)
      all(grepl(pattern, x, ignore.case = ignore.case, perl = perl))
  ),
  list(
    "starts_with",
    "Not every entry of {{.}} starts with {{.expr$prefix}}",
    function(x, prefix, na.rm = FALSE)
      all(startsWith(as.character(x), prefix), na.rm = na.rm)
  ),
  list(
    "ends_with",
    "Not every entry of {{.}} ends with {{.expr$suffix}}",
    function(x, suffix, na.rm = FALSE)
      all(endsWith(as.character(x), suffix), na.rm = na.rm)
  )
)
predicates$property <- list(
  list(
    "empty",
    "{{.}} is not empty",
    is_empty
  ),
  list(
    "not_empty",
    "{{.}} is empty",
    function(x) length(x) != 0
  ),
  list(
    "singleton",
    "{{.}} is not a singleton",
    function(x) length(x) == 1
  ),
  list(
    "not_na",
    "{{.}} is NA",
    function(x) !is_na(x)
  ),
  list(
    "without_na",
    "{{.}} has an NA",
    function(x) !anyNA(x)
  ),
  list(
    "named",
    "{{.}} is not named",
    is_named
  ),
  list(
    "has_name",
    "{{.}} does not have name {{.value$nm}}",
    function(x, nm) isTRUE(nm %in% names(x))
  ),
  list(
    "has_names",
    "{{.expr$nms}} are not all names of {{.}}",
    function(x, nms) all(nms %in% names(x))
  ),
  list(
    "has_length",
    "{{.}} is not of length {{.value$n}}",
    function(x, n) length(x) == n
  ),
  list(
    "has_attr",
    "{{.}} does not have attribute {{.value$which}}",
    function(x, which) !is.null(attr(x, which, exact = TRUE))
  ),
  list(
    "has_attrs",
    "{{.expr$which}} are not all attributes of {{.}}",
    function(x, which) all(which %in% names(attributes(x)))
  ),
  list(
    "inherits",
    '{{.}} is not of class "{{.value$what}}"',
    function(x, what) inherits(x, what)
  )
)
predicates$relation <- list(
  list(
    "identical",
    "{{.}} is not identical to {{.expr$to}}",
    function(x, to) identical(x, to)
  ),
  list(
    "not_identical",
    "{{.}} is identical to {{.expr$to}}",
    function(x, to) !identical(x, to)
  ),
  list(
    "equal",
    "{{.}} does not equal {{.expr$to}}",
    function(x, to) isTRUE(all.equal(to, x))
  ),
  list(
    "not_equal",
    "{{.}} equals {{.expr$to}}",
    function(x, to) !isTRUE(all.equal(to, x))
  ),
  list(
    "equivalent",
    "{{.}} is not equivalent to {{.expr$to}}",
    function(x, to) isTRUE(all.equal(to, x, check.attributes = FALSE))
  ),
  list(
    "not_equivalent",
    "{{.}} is equivalent to {{.expr$to}}",
    function(x, to) !isTRUE(all.equal(to, x, check.attributes = FALSE))
  ),
  list(
    "gt",
    "{{.}} is not greater than {{.value$lwr}}",
    function(x, lwr, na.rm = FALSE) all(x > lwr, na.rm = na.rm)
  ),
  list(
    "lt",
    "{{.}} is not less than {{.value$upr}}",
    function(x, upr, na.rm = FALSE) all(x < upr, na.rm = na.rm)
  ),
  list(
    "gte",
    "{{.}} is not greater than or equal to {{.value$lwr}}",
    function(x, lwr, na.rm = FALSE) all(x >= lwr, na.rm = na.rm)
  ),
  list(
    "lte",
    "{{.}} is not less than or equal to {{.value$upr}}",
    function(x, upr, na.rm = FALSE) all(x <= upr, na.rm = na.rm)
  )
)
predicates$set <- list(
  list(
    "in",
    "{{.}} is not in {{.expr$set}}",
    function(x, set) isTRUE(x %in% set)
  ),
  list(
    "not_in",
    "{{.}} is in {{.expr$set}}",
    function(x, set) isTRUE(! x %in% set)
  ),
  list(
    "include",
    "{{.}} does not include {{.expr$set}}",
    function(x, set) all(set %in% x)
  ),
  list(
    "exclude",
    "{{.}} intersects {{.expr$set}}",
    function(x, set) all(! set %in% x)
  ),
  list(
    "within",
    "{{.}} is not contained in {{.expr$set}}",
    function(x, set) all(x %in% set)
  ),
  list(
    "intersect",
    "{{.}} is disjoint from {{.expr$set}}",
    function(x, set) length(intersect(x, set)) != 0
  ),
  list(
    "avoid",
    "{{.}} intersects {{.expr$set}}",
    function(x, set) length(intersect(x, set)) == 0
  ),
  list(
    "setequal",
    "{{.}} and {{.expr$set}} are not equal as sets",
    function(x, set) setequal(x, set)
  )
)
make_predicate_data <- function(ns, xs, prefix, env) {
  unname(
    Map(function(nm, this) {
      list(
        nm,
        paste("{{.}} is not", this),
        getExportedValue(ns, paste0(prefix, nm)),
        env = env
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
  double     = "a double vector{{of_length(.value$n)}}",
  character  = "a character vector{{of_length(.value$n)}}",
  raw        = "a raw vector{{of_length(.value$n)}}"
)
env_formatter <- new.env(parent = baseenv())
env_formatter$of_length <- evalq(
  function(n) if (is.null(n)) "" else paste(" of length", n),
  env_formatter
)
predicates$type <- c(
  list(
    list(
      "not_null",
      "{{.}} is NULL",
      function(x) !is.null(x)
    ),
    list(
      "closure",
      "{{.}} is not a closure",
      is_closure
    ),
    list(
      "language",
      "{{.}} is not of type 'language'",
      function(x) typeof(x) == "language"
    ),
    list(
      "numerical",
      "{{.}} is not a numerical vector{{of_length(.value$n)}}",
      function(x, n = NULL) {
        if (! typeof(x) %in% c("double", "integer"))
          return(FALSE)
        if (!is.null(n) && length(x) != n)
          return(FALSE)
        TRUE
      },
      env = env_formatter
    ),
    list(
      "integerish",
      "{{.}} is not an integerish vector{{of_length(.value$n)}}",
      function(x, n = NULL) {
        if (! typeof(x) %in% c("double", "integer"))
          return(FALSE)
        if (!is.null(n) && length(x) != n)
          return(FALSE)
        all(x == as.integer(x))
      },
      env = env_formatter
    ),
    list(
      "complex",
      "{{.}} is not a complex vector{{of_length(.value$n)}}",
      function(x, n = NULL) {
        if (typeof(x) != "complex")
          return(FALSE)
        if (!is.null(n) && length(x) != n)
          return(FALSE)
        TRUE
      },
      env = env_formatter
    ),
    list(
      "number",
      "{{.}} is not a number",
      function(x)
        typeof(x) %in% c("double", "integer") && length(x) == 1 && !is.na(x)
    ),
    list(
      "boolean",
      "{{.}} is not a boolean",
      function(x)
        is.logical(x) && length(x) == 1 && !is.na(x)
    ),
    list(
      "string",
      "{{.}} is not a string",
      function(x)
        is.character(x) && length(x) == 1 && !is.na(x)
    )
  ),
  make_predicate_data("base", types_base, "is.", baseenv()),
  make_predicate_data("rlang", types_rlang, "is_", env_formatter)
)

for (x in unlist(predicates, recursive = FALSE)) {
  nm <- paste0("vld_", x[[1]])
  pred <- as_closure(x[[3]])
  vld_err_msg(pred) <- new_quosure(x[[2]], x$env %||% baseenv())
  assign(nm, pred)
}

#' @rawNamespace exportPattern("^vld_.+$")
NULL

# Documentation -----------------------------------------------------------

nms_predicates <- lapply(predicates, function(x) {
  nms <- vapply(x, `[[`, character(1), 1)
  paste0("vld_", nms)
})
# Order types as they appear in the "R Language Definition" manual
nms_predicates$type <- c(
  "vld_null",
  "vld_not_null",
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

#' Boolean predicates
#'
#' @evalRd rd_alias(nms_predicates$boolean)
#' @evalRd rd_usage(nms_predicates$boolean)
#'
#' @param x Object to test.
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
#' bar <- firmly(f, !! error_msg := vld_all_map(function(.) length(.) != 0, x))
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
#' @name predicates-boolean
NULL

#' Object predicates
#'
#' @evalRd rd_alias(nms_predicates$object)
#' @evalRd rd_usage(nms_predicates$object)
#'
#' @param x Object to test.
#'
#' @examples
#' row_sums <- firmly(rowSums, vld_matrix(x))
#' row_sums(matrix(1:6, 2, 3))
#'
#' ## Meaningless to sum across rows when column units differ
#' \dontrun{
#' row_sums(mtcars)}
#'
#' @seealso [Type predicates][predicates-type]
#'
#' @name predicates-object
NULL

#' Pattern predicates
#'
#' @evalRd rd_alias(nms_predicates$pattern)
#' @evalRd rd_usage(nms_predicates$pattern)
#'
#' @param x Object to test.
#' @param pattern Regular expression.
#' @param ignore.case Should pattern matching ignore case?
#' @param perl Should Perl-compatible regular expressions be used?
#' @param prefix,suffix String to match.
#' @param na.rm Should `NA` values be disregarded?
#'
#' @details To maintain consistency with [grepl()], `vld_starts_with()` and
#'   `vld_ends_with()` coerce to `character` before matching.
#'
#' @seealso [grepl()], [startsWith()], [endsWith()]
#'
#' @examples
#' ymd <- function(y, m, d) paste(y, m, d, sep = "/")
#'
#' too_old <- "Not a 21st-century year"
#' recent_ymd <- firmly(ymd, !! too_old := vld_grepl("^20[[:digit:]]{2}$", y))
#'
#' recent_ymd(2017, 01, 01)
#' \dontrun{
#' recent_ymd(1999, 01, 01)}
#'
#' way_too_old <- "Pre-2010 year is too old"
#' more_recent_ymd <- firmly(ymd, !! way_too_old := vld_starts_with("201", y))
#'
#' more_recent_ymd(2017, 01, 01)
#' \dontrun{
#' more_recent_ymd(2001, 01, 01)}
#'
#' @name predicates-pattern
NULL

#' Property predicates
#'
#' @evalRd rd_alias(nms_predicates$property)
#' @evalRd rd_usage(nms_predicates$property)
#'
#' @param x Object to test.
#' @param nm,nms Name(s).
#' @param n Length.
#' @param which Object attribute(s).
#' @param what Class name.
#'
#' @seealso [Set predicates][predicates-set],
#'   [vld_null()], [vld_not_null()]
#'
#' @examples
#' f <- function(x, y) NULL
#' foo <- firmly(f, "x, y are not disjoint" := vld_empty(intersect(x, y)))
#' foo(letters[1:3], letters[4:5])
#' \dontrun{
#' foo(letters[1:3], letters[3:5])}
#'
#' @name predicates-property
NULL

#' Relational predicates
#'
#' @evalRd rd_alias(nms_predicates$relation)
#' @evalRd rd_usage(nms_predicates$relation)
#'
#' @param x Object to test.
#' @param to Reference object.
#' @param lwr,upr Lower/upper bound.
#' @param na.rm Should `NA` values be disregarded?
#'
#' @seealso [all.equal()], [identical()]
#'
#' @examples
#' f <- function(x, y) log(y - x) / log(x)
#' foo <- firmly(f, vld_gt(0, x - 1, "y not greater than x" := y - x))
#' foo(2, 4)
#' \dontrun{
#' foo(1, 2)
#' foo(2, 2)}
#'
#' @name predicates-relational
NULL

#' Set predicates
#'
#' @evalRd rd_alias(nms_predicates$set)
#' @evalRd rd_usage(nms_predicates$set)
#'
#' @param x Object to test.
#' @param set Reference set (as a vector).
#'
#' @seealso [Set operations][setequal()],
#'   [Property predicates][predicates-property]
#'
#' @examples
#' s3methods <- function(x) {
#'   methods <- attr(methods(class = class(x)), "info")
#'   with(methods, generic[!isS4])
#' }
#' foo <- fasten(
#'   vld_include("predict", s3methods(object))
#' )(
#'   function(object, data) {
#'     pred <- predict(object, data)
#'     "Do something with prediction"
#'   }
#' )
#'
#' mdl <- lm(mpg ~ wt, mtcars[1:10, ])
#' data <- mtcars[11:12, ]
#'
#' foo(mdl, data)
#' \dontrun{
#' foo(NULL, data)}
#'
#' @name predicates-set
NULL

#' Type predicates
#'
#' @evalRd rd_alias(nms_predicates$type)
#' @evalRd rd_usage(nms_predicates$type)
#'
#' @param x Object to test.
#' @param n Length.
#' @param encoding Encoding of a string or character vector. One of `UTF-8`,
#'   `latin1`, or `unknown`.
#'
#' @seealso
#'   - [rlang type predicates][rlang::type-predicates], which underlie the
#'     length-dependent predicates (except `vld_numerical()`)
#'   - [Object predicates][predicates-object], for verifying identities that are
#'     not characterized by type, e.g., data frames, which have type `list`
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
#' @name predicates-type
NULL
