# Pipe operator
`%>%` <- magrittr::`%>%`

# Null-default operator
`%||%` <- purrr::`%||%`

is_true  <- isTRUE
is_false <- function(x) identical(FALSE, x)

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
      purrr::map_chr(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]])),
      collapse = ""
    )
  } else {
    paste0(x, "\n")
  }
}
