suppressMessages(library(stringr))
suppressMessages(library(purrr))

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

make_fnc <- function(args, body = quote(NULL), env = parent.frame()) {
  args <- as.pairlist(args)
  f <- eval(call("function", args, body))
  environment(f) <- env
  f
}

# Make a function that simply passes the inputs as a list
#
# If the output of strictly() of such a dummy function matches the output of the
# dummy function, we can be sure that strictly() exactly reproduces the behavior
# of any function with argument signature identical to the dummy function's.
pass_args <- function(args) {
  body <- substitute({
    call <- match.call()
    call[[1L]] <- as.name("list")
    eval(call)
  })
  make_fnc(args, body)
}

#' Expect exactly n errors matching pattern
expect_n_errors <- function(n, f, args, pattern) {
  do.call(purrr::safely(f), args) %>% {
    stringr::str_count(.$error, pattern)
  } %>%
    expect_equal(n)
}
