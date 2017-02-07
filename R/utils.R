# Pipe operator
`%>%` <- magrittr::`%>%`

# Null-default operator
`%||%` <- purrr::`%||%`

skip <- function(...) {}

# Aliases consistent with our style convention
is_true <- isTRUE
is_false <- function(x) identical(FALSE, x)

#' Collapse a character vector into an enumerated string
#'
#' @param x Character vector.
#' @param many Integer. How many is too many?
#' @return String.
#' @keywords internal
enumerate_many <- function(x, many = 2L) {
  if (length(x) >= many) {
    paste(
      vapply(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]]),
             character(1)),
      collapse = ""
    )
  } else {
    paste0(x, "\n")
  }
}
