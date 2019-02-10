`%>%` <- local({
  as_curried_fn <- function(expr, env) {
    if (!is.call(expr))
      return(match.fun(expr))
    if (no_dot_arg(expr))
      expr <- curry_args(expr)
    lambda(body = expr, env)
  }

  lambda <- function(body, env) {
    expr_fn <- bquote(function(.) .(body))
    f <- eval(expr_fn)
    environment(f) <- env
    f
  }

  no_dot_arg <- function(call) {
    all(args(call) != quote(.))
  }

  curry_args <- function(call) {
    call_dot <- c(call[[1]], quote(.), args(call))
    as.call(call_dot)
  }

  args <- function(call) {
    as.list(call)[-1]
  }

  function(lhs, rhs) {
    expr <- substitute(rhs)
    f <- as_curried_fn(expr, parent.frame())
    f(lhs)
  }
})
