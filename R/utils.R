# Null-default operator
`%||%` <- purrr::`%||%`

#' Represent non-dot arguments by name and symbol
#'
#' @param sig Pairlist.
#' @return List with components \code{"nm"} (character), \code{"symb"} (symbol),
#'   \code{"has_val"} (logical).
#' @keywords internal
repn_args <- function(sig) {
  nm <- setdiff(names(sig), "...") %||% character(0)
  list(
    nm        = nm,
    symb      = lapply(nm, as.name),
    has_value = vapply(sig[nm], `!=`, logical(1), substitute(),
                       USE.NAMES = FALSE)
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