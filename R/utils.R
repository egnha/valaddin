`%||%` <- function(x, y) {
  if (is_empty(x)) y else x
}

try_eval_tidy <- function(expr, env = parent.frame()) {
  tryCatch(
    eval_tidy(expr, env = env),
    error = identity
  )
}

check_is_class <- function(cls) {
  force(cls)
  function(x)
    inherits(x, cls)
}
is_error <- check_is_class("error")
# stricter and slightly faster than rlang::is_string()
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

# Substitute string into call, to avoid making a binding that could take
# precedence over those in higher environments
glue_text <- function(text, env, data = NULL, ...) {
  eval(bquote(glue::glue_data(.x = data, .(text), .envir = env, ...)))
}

deparse_collapse <- function(x) {
  d <- deparse(x)
  if (length(d) > 1)
    paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  else
    d
}

enumerate_many <- function(x, many = 2) {
  if (length(x) >= many)
    enumerate(x)
  else
    paste0(x, "\n")
}
enumerate <- function(x) {
  enumerations <-
    vapply(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]]), "")
  paste(enumerations, collapse = "")
}

nomen <- function(x) {
  nms <- names_nondot(x)
  names(nms) <- nms
  lapply(nms, as.name)
}
names_nondot <- function(x) {
  names(x)[names(x) != "..."] %||% character(0)
}
