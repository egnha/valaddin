# Pipe operator
`%>%` <- magrittr::`%>%`

# Null-default operator
`%||%` <- purrr::`%||%`

is_true  <- isTRUE
is_false <- function(x) identical(FALSE, x)

is_void_symb <- function(x) {
  is.symbol(x) && x == substitute()
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
