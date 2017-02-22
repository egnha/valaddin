# Null-default operator
`%||%` <- purrr::`%||%`

is_true <- isTRUE
is_false <- function(x) identical(FALSE, x)
is_error <- function(x) inherits(x, "error")

skip <- function(...) {}

# Deparse a language object as a single string
deparse_collapse <- function(x) {
  paste(trimws(deparse(x), which = "both"), collapse = "")
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
