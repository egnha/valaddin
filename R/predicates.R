# Non-bare "numeric" predicates are omitted from rlang 0.1.1
is_numeric <- function(x, n = NULL) {
  if (!typeof(x) %in% c("double", "integer"))
    return(FALSE)
  if (!is.null(n) && length(x) != n)
    return(FALSE)
  TRUE
}
is_scalar_numeric <- function(x) {
  is_numeric(x) && length(x) == 1
}
is_number <- is_scalar_numeric
