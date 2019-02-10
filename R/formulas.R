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
f_eval_lhs <- f_evaluator(lazyeval::f_lhs)
f_eval_rhs <- f_evaluator(lazyeval::f_rhs)
