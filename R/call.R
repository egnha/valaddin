express_lambda <- function(body) {
  call("function", as.pairlist(alist(. = )), body)
}

deparse_call <- function(chk_item, fn_expr) {
  call <- substitute(f(x), list(f = fn_expr, x = lazyeval::f_rhs(chk_item)))
  deparse_collapse(call)
}

# Call a function against a given call signature
call_fn <- function(.f) {
  force(.f)

  function(.call) {
    .call[[1L]] <- .f
    .call
  }
}

# Rewire the argument signature of a function
with_sig <- function(.f, .sig, .attrs) {
  f <- eval(call("function", .sig, body(.f)))
  environment(f) <- environment(.f)
  attributes(f)  <- .attrs
  f
}
