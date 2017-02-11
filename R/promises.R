#' Assign symbols to an environment as promises
#'
#' @param symbs List of symbols.
#' @param env_eval Common environment in which \code{symbs} are to be evaluated.
#' @param env_asgn Environment to which \code{symbs} are to be bound.
#' @return The environment \code{env_asgn}, invisibly.
#' @keywords internal
lazy_assign <- function(symbs, env_eval, env_asgn) {
  for (x in symbs) {
    eval(substitute(
      delayedAssign(deparse(x), ..expr.., env_eval, env_asgn),
      list(..expr.. = x)
    ))
  }

  invisible(env_asgn)
}