# Empty-default operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

# Hush `R CMD check` note (confused by `.` in pipes)
. <- NULL

is_true <- isTRUE
is_false <- function(x) identical(FALSE, x)

stop_wo_call <- function(...) stop(..., call. = FALSE)
warning_wo_call <- function(...) warning(..., call. = FALSE)

# Deparse a language object as a single string
deparse_collapse <- function(x) {
  d <- deparse(x)
  if (length(d) > 1L) {
    paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  } else {
    d
  }
}

# Typically used to list symbols, such as function argument names
quote_collapse <- function(xs) {
  paste(encodeString(xs, quote = "`"), collapse = ", ")
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
