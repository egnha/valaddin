library(stringr, warn.conflicts = FALSE)

# list() as a functor (cf. purrr::lift())
lift <- function(f) {
  function(args) do.call(f, args)
}

# Error monad (cf. purrr::safely())
safely <- function(f) {
  function(...) {
    tryCatch(
      list(result = f(...), error = NULL),
      error = function(e) list(result = NULL, error = e)
    )
  }
}

# Cf. purrr::map_lgl()
map_lgl <- local({
  as_function <- function(f) {
    if (is.function(f))
      return(f)
    # Otherwise, assume f is a one-sided formula
    new_fn(alist(. = ), f[[2]], environment(f))
  }

  function(xs, f, ...) {
    f <- as_function(f)
    vapply(xs, f, ..., FUN.VALUE = logical(1))
  }
})

new_fn <- function(args, body = quote(NULL), env = parent.frame()) {
  f <- eval(call("function", as.pairlist(args), body))
  environment(f) <- env
  f
}

noneval_checks <- list(
  x ~ is.numeric,
  log(x) ~ is.numeric,
  list(x) ~ is.numeric,
  list(x, ~y) ~ is.numeric,
  is.numeric ~ x,
  list(~is.numeric) ~ x,
  ~ is.numeric(x)
)

invalid_checks <- list(
  NULL ~ is.numeric,
  log ~ is.numeric,
  log(1) ~ is.numeric,
  {~x} ~ is.numeric,
  as.name("not_a_string") ~ is.numeric,
  list() ~ is.numeric,
  list(1 ~ x) ~ is.numeric,
  list(log ~ x) ~ is.numeric,
  list(NULL ~ x) ~ is.numeric,
  list(NA ~ x) ~ is.numeric,
  list(as.name("not_a_string") ~ x) ~ is.numeric,
  list(list(~x)) ~ is.numeric,
  list(list("x not numeric" ~ x)) ~ is.numeric,
  ~ "not_a_function",
  ~ as.name("not_a_function"),
  ~ NULL,
  ~ NA,
  ~ ~{.}
)

args_list <- list(
  alist(),
  alist(... = ),
  alist(x =  ),
  alist(x = 0),
  alist(x =  , ... = ),
  alist(x = 0, ... = ),
  alist(x =  , y =  ),
  alist(x =  , y = 1),
  alist(x = 0, y = 1),
  alist(x =  , y =  , ... = ),
  alist(x =  , y = 1, ... = ),
  alist(x = 0, y = 1, ... = ),
  alist(x =  , y =  , z = x + y),
  alist(x = 0, y =  , z = x + y),
  alist(x = 0, y = 1, z = x + y),
  alist(x =  , y =  , z = x + y, ... = ),
  alist(x = 0, y =  , z = x + y, ... = ),
  alist(x = 0, y = 1, z = x + y, ... = )
)

# Make a function that simply passes the inputs as a list
#
# If the output of firmly() of such a dummy function matches the output of the
# dummy function, we can be sure that firmly() exactly reproduces the behavior
# of any function with argument signature identical to the dummy function's.
pass_args <- function(args) {
  body <- substitute({
    call <- match.call()
    call[[1L]] <- as.name("list")
    eval(call)
  })
  new_fn(args, body)
}

# Expect exactly n errors matching pattern
expect_n_errors <- function(n, f, args, pattern) {
  suppressWarnings(do.call(safely(f), args, quote = TRUE)) %>% {
    stringr::str_count(as.character(.$error), pattern)
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