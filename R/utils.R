# Pipe operator
`%>%` <- magrittr::`%>%`

# Null-default operator
`%||%` <- purrr::`%||%`

skip <- function(...) {}

# Aliases consistent with our style convention
is_true <- isTRUE
is_false <- function(x) identical(FALSE, x)

# An error object inherits from "condition"
is_error <- function(x) {
  wh <- inherits(x, c("error", "condition"), which = TRUE)
  wh[1L] > 0L && wh[2L] > wh[1L]
}

# Collapse a character vector into an enumerated string
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
