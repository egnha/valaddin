chk_error_class <- vld(
  "'error_class' must be NULL or a character vector without NAs" :=
    {is.null(.) || is.character(.) && !anyNA(.)} ~ error_class
)
fasten_ <- function(..., error_class = NULL) {
  chk_parts <- parse_checks(vld(...))
  error_class <- error_class[nzchar(error_class)]
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    error_class_na <- is.null(firm_checks(f)) || !length(error_class)
    if (!length(arg$nm) || is.null(chk_parts) && error_class_na) {
      return(f)
    }
    chks <-
      do.call("rbind", c(
        list(firm_checks(f)),
        list(chk_parts$local),
        list(localize_at(chk_parts$global, arg$sym))
      ))
    chks <- chks[rev(!duplicated(rev(chks$call))), , drop = FALSE]
    error_class <- error_class %||% firm_error(f) %||% "inputValidationError"
    as_firm_closure(
      with_sig(
        validation_closure(loosely(f), chks, sig, arg, error_class),
        sig,
        attributes(f)
      )
    )
  }
}
nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, sym = lapply(nm, as.symbol))
}
#' @export
loosely <- function(f) {
  if (!is.function(f))
    stop("'f' must be a function", call. = FALSE)
  else if (is_firm(f))
    firm_core(f)
  else
    f
}
#' @export
is_firm <- function(x) {
  inherits(x, "firm_closure")
}
as_firm_closure <- function(f) {
  if (!is_firm(f))
    class(f) <- c("firm_closure", class(f))
  f
}
with_sig <- function(f, sig, attrs) {
  formals(f) <- sig
  attributes(f) <- attrs
  f
}

#' @export
fasten <- fasten_(UQS(chk_error_class))(fasten_)

#' @export
firmly <- fasten(
  "'f' must be a function" := is.function ~ f,
  UQS(chk_error_class)
)(
  function(f, ..., error_class = NULL) {
    if (is.primitive(f))
      f <- rlang::as_closure(f)
    fasten_(..., error_class = error_class)(f)
  }
)

#' @export
print.firm_closure <- function(x, ...) {
  cat("<firm_closure>\n")
  cat("\n* Core function:\n")
  print.default(firm_core(x))
  cat("\n* Checks (<predicate>:<error message>):\n")
  chks <- firm_checks(x)
  if (length(chks)) {
    labels <- paste0(chks$call, ":\n", encodeString(chks$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }
  cat("\n* Error subclass for check errors:\n")
  subclass <- firm_error(x)
  if (!is.null(subclass))
    cat(paste(subclass, collapse = ", "), "\n")
  else
    cat("None\n")
  invisible(x)
}

#' Validate objects
#'
#' @param . Object to validate.
#' @param ... Input validation checks.
#' @param error_class Character vector of the error subclass to signal if
#'   validation fails. If `NULL` (the default), the error subclass is
#'   `objectValidationError`.
#'
#' @examples
#' # All assertions valid: data frame returned (invisibly)
#' validate(mtcars,
#'          is.data.frame,
#'          vld_all_map(is.numeric),
#'          vld_gt(10)(nrow(.)),
#'          vld_has_names(c("mpg", "cyl"))(.))
#'
#' \dontrun{
#' # Some assertions invalid: diagnostic error raised
#' validate(mtcars,
#'          is.matrix,
#'          vld_all_map(is.numeric),
#'          vld_gt(1000)(nrow(.)),
#'          vld_has_name("cylinders")(.))}
#'
#' @export
validate <- fasten(
  UQS(chk_error_class)
)(
  function(., ..., error_class = NULL) {
    validate <-
      loosely(validify)(..., error_class = error_class)
    eval(bquote(validate(.(substitute(.)))))
  }
)
#' @rdname validate
#' @export
validify <- fasten(
  UQS(chk_error_class)
)(
  function(..., error_class = NULL) {
    error_class <- error_class %||% "objectValidationError"
    structure(
      fasten_(..., error_class = error_class)(pass),
      class = c("validator", "firm_closure", "function")
    )
  }
)
# name of argument must coincide with name of validate()'s object-argument
pass <- function(.) invisible(.)

#' @export
print.validator <- function(x, ...) {
  cat("<validator>\n")
  cat("\n* Validation (<predicate>:<error message>):\n")
  chks <- firm_checks(x)
  if (length(chks)) {
    labels <- paste0(chks$call, ":\n", encodeString(chks$msg, quote = "\""))
    cat(enumerate_many(labels))
  } else {
    cat("None\n")
  }
  cat("\n* Error subclass for validation errors:\n")
  subclass <- firm_error(x)
  if (!is.null(subclass))
    cat(paste(subclass, collapse = ", "), "\n")
  else
    cat("None\n")
  invisible(x)
}
