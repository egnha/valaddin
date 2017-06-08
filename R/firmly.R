#' @importFrom rlang quos
#' @export
rlang::quos

chks_vld <- rlang::quos(
  "'checklist' must be a list of quosures" =
    {all(vapply(., rlang::is_quosure, logical(1)))} ~ checklist,
  "'error_class' must be a character vector without NAs" =
    {is.character(.) && !anyNA(.)} ~ error_class
)

`_vld` <- function(..., checklist = list(), error_class = character()) {
  chks <- c(rlang::quos(...), checklist)
  error_class <- error_class[nzchar(error_class)]
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    error_class_na <- is.null(firm_checks(f)) || !length(error_class)
    if (!length(arg[["nm"]]) || !length(chks) && error_class_na) {
      return(f)
    }
    chks <-
      do.call("rbind", c(
        firm_checks(f),
        Map(function(chk, msg) parse_check(chk, msg, arg[["sym"]]),
            chks, names_filled(chks))
      ))
    chks <- chks[rev(!duplicated(rev(chks[["call"]]))), , drop = FALSE]
    error_class <- error_class %||% firm_error(f) %||% "inputValidationError"
    as_firm_closure(with_sig(
      validation_closure(loosely(f), chks, sig, arg, error_class),
      sig, attributes(f)
    ))
  }
}

nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, sym = lapply(nm, as.symbol))
}

#' @export
loosely <- function(f) {
  if (!is.function(f)) {
    stop("'f' must be a function", call. = FALSE)
  } else if (is_firm(f)) {
    firm_core(f)
  } else {
    f
  }
}

is_firm <- function(x) {
  inherits(x, "firm_closure")
}

as_firm_closure <- function(f) {
  if (!is_firm(f)) {
    class(f) <- c("firm_closure", class(f))
  }
  f
}

with_sig <- function(f, sig, attrs) {
  formals(f) <- sig
  attributes(f) <- attrs
  f
}

#' @export
vld <- `_vld`(checklist = chks_vld)(`_vld`)

#' @export
firmly <- vld(
  "'f' must be a closure" = rlang::is_closure ~ f,
  checklist = chks_vld
)(function(f, ..., checklist = list(), error_class = character()) {
  `_vld`(..., checklist = checklist, error_class = error_class)(f)
})

#' @export
`%checkin%` <- function(chks, f) {
  # identity function of rhs, provisionally
  f
}

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
  if (!is.null(subclass)) {
    cat(paste(subclass, collapse = ", "), "\n")
  } else {
    cat("None\n")
  }
}
