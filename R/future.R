# Provisional package object: lazyeval::f_new(), 0.2.0.9000
#
# Until this version of f_new() finds its way into the CRAN version of lazyeval
# (or rlang), we need to include it, explicitly, for only this version of
# f_new() doesn't require rhs/lhs be language objects; sometimes we want these
# to be object bindings, cf. scope.R.

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
