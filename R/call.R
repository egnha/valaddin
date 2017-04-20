express_lambda <- function(body) {
  call("function", as.pairlist(alist(. = )), body)
}

deparse_call <- function(chk_item, fn_expr) {
  call <- substitute(f(x), list(f = fn_expr, x = lazyeval::f_rhs(chk_item)))
  deparse_collapse(call)
}

# Rewire the argument signature of a function
with_sig <- function(.f, .sig, .attrs) {
  formals(.f)    <- .sig
  attributes(.f) <- .attrs
  .f
}
