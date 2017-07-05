library(stringr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

errmsg_false <- function(text) {
  esc_perl(message_false(text))
}
errmsg_error <- function(text) {
  esc_perl(sprintf("Error evaluating check %s", text))
}
errmsg_invalid <- function(call, out) {
  esc_perl(err_invalid_value(call, out))
}

# Expect exactly n errors matching pattern
expect_n_errors <- function(n, f, args, pattern) {
  suppressWarnings(do.call(purrr::safely(f), args, quote = TRUE)) %>% {
    stringr::str_count(.$error, pattern)
  } %>%
    expect_equal(n)
}

# Escape string for Perl-style regex
esc_perl <- function(x) stringr::str_replace_all(x, "(\\W)", "\\\\\\1")

# Adapt an expectation function to use Perl-style regex
perlize <- function(f) {
  force(f)

  function(object, regexp, ...) {
    f(object, esc_perl(regexp), perl = TRUE, ...)
  }
}