# Formula functions from future lazyeval (> 0.2.0) or rlang
#
# lazyeval::f_new is too rigid in versions <= 0.2.0, where rhs and lhs are
# required to be language objects; in valaddin, rhs, resp. lhs, is typically a
# function, resp. list. Therefore these "future" formula functions are
# provisionally included until they, or some functional analogue of them, become
# available on CRAN (either in lazyeval or its fork, rlang).

# Future f_new (available in development version 0.2.0.9000 of lazyeval)
ff_new <- function(rhs, lhs = NULL, env = parent.frame()) {
  if (!is.environment(env)) {
    stop_wo_call("`env` must be an environment")
  }

  f <- if (is.null(lhs)) {
    lazyeval::call_new("~", rhs)
  } else {
    lazyeval::call_new("~", lhs, rhs)
  }

  structure(f, class = "formula", .Environment = env)
}

f_evaluator <- function(fexpr) {
  force(fexpr)

  function(f) {
    if (!lazyeval::is_formula(f)) {
      stop_wo_call("`f` is not a formula")
    }

    eval(fexpr(f), lazyeval::f_env(f))
  }
}

# Simplified f_lhs, f_rhs, independent of lazyeval::f_new
ff_eval_lhs <- f_evaluator(lazyeval::f_lhs)
ff_eval_rhs <- f_evaluator(lazyeval::f_rhs)

# Future-dependent `f_lhs<-`
`ff_lhs<-` <- function(x, value) {
  if (!lazyeval::is_formula(x)) {
    stop_wo_call("`x` is not a formula")
  }

  ff_new(lazyeval::f_rhs(x), value, lazyeval::f_env(x))
}
