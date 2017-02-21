# Null-default operator
`%||%` <- purrr::`%||%`

# Aliases consistent with our style convention
is_true <- isTRUE
is_false <- function(x) identical(FALSE, x)

skip <- function(...) {}

is_error <- function(x) {
  wh <- inherits(x, c("error", "condition"), which = TRUE)
  wh[1L] > 0L && wh[2L] > wh[1L]
}

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

# Identical to lazyeval::f_new from dev version 0.2.0.9000
# We need this because, unlike the CRAN version of lazyeval::f_new (0.2.0),
# it does not require that rhs/lhs be language objects, for sometimes we want
# these to be object bindings (for example, in scope.R).
f_new <- function(rhs, lhs = NULL, env = parent.frame()) {
  if (!is.environment(env)) {
    stop("`env` must be an environment", call. = FALSE)
  }

  f <- if (is.null(lhs))
    lazyeval::call_new("~", rhs)
  else
    lazyeval::call_new("~", lhs, rhs)

  structure(f, class = "formula", .Environment = env)
}