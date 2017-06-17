# Empty-default operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

names_filled <- function(x) {
  names(x) %||% character(length(x))
}

# Substitute string into call to avoid binding string to env,
# which could clash with a name in an environment higher up.
glue_text <- function(text, env, data = NULL, ...) {
  eval(bquote(glue::glue_data(.x = data, .(text), .envir = env, ...)))
}

# Pipe operator
`%>%` <- purrr::`%>%`

is_error <- function(x) inherits(x, "error")

# Deparse a language object as a single string
deparse_collapse <- function(x) {
  d <- deparse(x)
  if (length(d) > 1) {
    paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  } else {
    d
  }
}


# Collapse a character vector into an enumerated string
enumerate_many <- function(x, many = 2) {
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
