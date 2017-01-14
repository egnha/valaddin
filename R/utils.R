# Pipe operator
`%>%` <- magrittr::`%>%`

# Null-default operator
`%||%` <- purrr::`%||%`

is_true  <- isTRUE
is_false <- function(x) identical(FALSE, x)

is_void_symb <- function(x) {
  is.symbol(x) && x == substitute()
}

#' Find first predicate in a list that returns \code{TRUE}
#'
#' @param predicates List of predicate functions.
#' @param x R object.
#' @param .no_true Value to return is no predicate returns \code{TRUE} on
#'   \code{x}.
#' @return Index of first predicate in \code{predicates} to return \code{TRUE}
#'   on \code{x}, otherwise \code{.no_true}.
#' @keywords internal
#' @examples
#' predicates <- list(is.null, function(.) is.logical(.) && is.na(.))
#' which_first_true(predicates, NULL)  # 1
#' which_first_true(predicates, NA)    # 2
#' which_first_true(predicates, 0)     # NULL
#' which_first_true(predicates, sin)   # NULL
which_first_true <- function(predicates, x, .no_true = NULL) {
  res <- Reduce(function(., p) . || p(x), predicates, FALSE, accumulate = TRUE)
  wh <- which.max(c(res[-1L], TRUE))

  if (wh == length(res)) .no_true else wh
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