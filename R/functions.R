# Call a function against a given call signature
call_fn <- function(.f) {
  force(.f)

  function(.call) {
    .call[[1L]] <- .f
    .call
  }
}

# Rewrite the argument signature of a function
with_sig <- function(.f, .sig, .attrs) {
  f <- eval(call("function", .sig, body(.f)))
  environment(f) <- environment(.f)
  attributes(f)  <- .attrs

  f
}