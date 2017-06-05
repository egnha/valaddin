#' @importFrom rlang quos
#' @export
rlang::quos

#' @export
firmly <- function(f, ..., checklist = list(), error_class = character()) {
  vld(..., checklist = checklist, error_class = error_class)(f)
}

#' @export
`%checkin%` <- function(chks, f) {
  # identity function of rhs, provisionally
  f
}

#' @export
vld <- function(..., checklist = list(), error_class = character()) {
  force(error_class)
  chks <- c(rlang::quos(...), checklist)
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    msgs <- names(chks) %||% character(length(chks))
    chks <- do.call(
      "rbind",
      Map(function(., ..) parse_check(., .., arg[["sym"]]), chks, msgs)
    )
    chks <- chks[rev(!duplicated(rev(chks$call))), , drop = FALSE]
    error_class <- error_class %||% firm_error(f) %||% "inputValidationError"
    validation_closure(f, chks, sig, arg[["nm"]], arg[["sym"]], error_class)
  }
}

nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, sym = lapply(nm, as.symbol))
}
