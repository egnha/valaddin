#' @export
stop_tersely <- function(cond, domain = NULL) {
  msg <- conditionMessage(cond)
#  .Internal(.signalCondition(cond, msg, NULL))
  .Internal(stop(FALSE, .makeMessage(msg, domain = domain)))
}
