library(stringr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

errmsg_false <- function(text) {
  esc_perl(message_false(text))
}
errmsg_error <- function(text) {
  esc_perl(paste("Error evaluating check", text))
}
errmsg_invalid <- function(call, out) {
  esc_perl(err_invalid_value(call, out))
}

# Adapt an expectation function to use Perl-style regex
perlize <- function(f) {
  force(f)

  function(object, regexp, ...) {
    f(object, esc_perl(regexp), perl = TRUE, ...)
  }
}

expect_error_perl <- function(...) {
  expect_error(..., perl = TRUE)
}

# Escape string for Perl-style regex
esc_perl <- function(x) stringr::str_replace_all(x, "(\\W)", "\\\\\\1")

# Expect exactly n errors matching pattern
expect_n_errors <- function(n, f, args, pattern) {
  suppressWarnings(do.call(purrr::safely(f), args, quote = TRUE)) %>% {
    stringr::str_count(.$error, pattern)
  } %>%
    expect_equal(n)
}

only <- function(x, not) {
  sprintf("(?!.*%s).*%s.*$", not, x)
}
both <- function(x, y) {
  # Need (?s:.)* because .* won't match newline as Perl regex
  sprintf("(%s(?s:.)*%s)|(%s(?s:.)*%s)", x, y, y, x)
}
only_false <- function(this, not_this) {
  only(errmsg_false(this), errmsg_false(not_this))
}
both_false <- function(this, that) {
  both(errmsg_false(this), errmsg_false(that))
}
