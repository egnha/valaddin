# Empty-default operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

names_filled <- function(x) {
  names(x) %||% character(length(x))
}

# When gluing, substitute string into call, to avoid making a binding that could
# inadvertently override one in an environment higher up.
glue_text <- function(text, env, data = NULL, ...) {
  eval(bquote(glue::glue_data(.x = data, .(text), .envir = env, ...)))
}

# Deparse a language object as a single string
deparse_collapse <- function(x) {
  d <- deparse(x)
  if (length(d) > 1) {
    paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  } else {
    d
  }
}

# base::trimws() is only available in R >= 3.2.0
trimws <- function(x, which = c("both", "left", "right")) {
  sub_ <- function(re, x) sub(re, "", x, perl = TRUE)
  switch(
    match.arg(which),
    left  = sub_("^[ \t\r\n]+", x),
    right = sub_("[ \t\r\n]+$", x),
    both  = sub_("[ \t\r\n]+$", sub_("^[ \t\r\n]+", x))
  )
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
