#' @importFrom rlang quos
#' @export
rlang::quos

nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(nm = nm, sym = lapply(nm, as.symbol))
}

firm_closure <- function(f) {
  if (!inherits(f, "firm_closure")) {
    class(f) <- c("firm_closure", class(f))
  }
  f
}

with_sig <- function(f, sig, attrs) {
  formals(f) <- sig
  attributes(f) <- attrs
  f
}

chks_vld <- rlang::quos(
  "'checklist' must be a list (of checks)" = is.list ~ checklist,
  "'error_class' must be character vector" = is.character ~ error_class
)

`_vld` <- function(..., checklist = list(), error_class = character()) {
  force(error_class)
  chks <- c(rlang::quos(...), checklist)
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    msgs <- names(chks) %||% character(length(chks))
    chks <- do.call(
      "rbind",
      Map(function(chk, msg) parse_check(chk, msg, arg[["sym"]]), chks, msgs)
    )
    chks <- chks[rev(!duplicated(rev(chks[["call"]]))), , drop = FALSE]
    error_class <- error_class %||% firm_error(f) %||% "inputValidationError"
    firm_closure(with_sig(
      validation_closure(f, chks, sig, arg, error_class),
      sig, attributes(f)
    ))
  }
}

#' @export
vld <- `_vld`(checklist = chks_vld)(`_vld`)

chks_firmly <- c(
  "'f' must be a closure" = rlang::quo(rlang::is_closure ~ f),
  chks_vld
)

`_firmly` <- function(f, ..., checklist = list(), error_class = character()) {
  `_vld`(..., checklist = checklist, error_class = error_class)(f)
}

#' @export
firmly <- `_vld`(checklist = chks_firmly)(`_firmly`)

#' @export
`%checkin%` <- function(chks, f) {
  # identity function of rhs, provisionally
  f
}
