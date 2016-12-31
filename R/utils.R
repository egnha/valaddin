# Null-default operator
`%||%` <- purrr::`%||%`

prepend_to_vec <- function(x, vec) {
  if (x %in% vec) vec else c(x, vec)
}

is_subset_vec <- function(x, l) {
  (is.logical(x) && length(x) == l) || (is.numeric(x) && all(x >= 1 & x <= l))
}

is_void_symb <- function(x) {
  is.symbol(x) && x == substitute()
}

is_null_or_logical <- function(x) {
  is.null(x) || purrr::is_scalar_logical(x)
}

#' Represent non-dot arguments by name and symbol
#'
#' @param sig Pairlist.
#' @return List with components \code{"nm"} (character), \code{"symb"} (symbol),
#'   \code{"wo_value"} (logical).
#' @keywords internal
nomen <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(
    nm       = nm,
    symb     = lapply(nm, as.symbol),
    wo_value = vapply(sig[nm], is_void_symb, logical(1), USE.NAMES = FALSE)
  )
}

#' Deparse a language object as a single string
#'
#' @param x Language object.
#' @return String.
#' @details Used in validation check template, must therefore be exported.
#' @keywords internal
#' @export
deparse_collapse <- function(x) {
  paste(trimws(deparse(x), which = "both"), collapse = "")
}

#' Enumerate a character vector as a string
#'
#' @param x Character vector.
#' @param many Integer. How many is too many?
#' @return String.
#' @details Used in validation check template, must therefore be exported.
#' @keywords internal
#' @export
enumerate_many <- function(x, many = 2L) {
  if (length(x) >= many) {
    paste(
      purrr::map_chr(seq_along(x), function(i) sprintf("[%d] %s\n", i, x[[i]])),
      collapse = ""
    )
  } else {
    paste0(x, "\n")
  }
}