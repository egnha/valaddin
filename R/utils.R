# Empty-default operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Infix attribute accessor
`%@%` <- rlang::`%@%`

try_eval_tidy <- function(expr, env = rlang::caller_env()) {
  tryCatch(
    rlang::eval_tidy(expr, env = env),
    error = identity
  )
}

# A quosure environment is for tidyeval, not introspection;
# therefore, it is not necessarily the calling environment,
# which needs to be tracked, separately.
# cf. https://github.com/tidyverse/rlang/issues/185
capture_env <- function(x, env) {
  env_x <- rlang::f_env(x)
  if (identical(env_x, emptyenv()))
    env
  else
    env_x
}

identify_caller <- function(nm) {
  caller <- as.name(nm)
  function(x)
    is.call(x) && identical(x[[1]], caller)
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
  if (length(d) > 1)
    paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  else
    d
}

# Collapse a character vector into an enumerated string
enumerate_many <- function(x, many = 2) {
  if (length(x) >= many)
    paste(
      vapply(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]]),
             character(1)),
      collapse = ""
    )
  else
    paste0(x, "\n")
}

comma_collapse <- function(x, width = 60) {
  x_str <- paste(x, collapse = ", ")
  if (nchar(x_str) > width)
    sprintf("%s ... [TRUNCATED]", substr(x_str, 1, width))
  else
    x_str
}
