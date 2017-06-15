# Empty-default operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

names_filled <- function(x) {
  names(x) %||% character(length(x))
}

const <- function(x) {
  force(x)
  function(...) x
}

glue_text <- function(text, env, data = NULL) {
  # substitute string into call to avoid binding string to env,
  # which could clash with a name in an environment higher up
  eval(bquote(glue::glue_data(.x = data, .(text), .envir = env))) %||%
    # work-around bug in glue 1.0.0: returns character(0) for ""
    ""
}

# Pipe operator
`%>%` <- purrr::`%>%`

# Hush `R CMD check` note (confused by `.` in pipes)
. <- NULL

is_true <- isTRUE
is_false <- function(x) identical(FALSE, x)
is_error <- function(x) inherits(x, "error")

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
