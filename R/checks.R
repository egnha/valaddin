is_valid_checklist <- function(...) {
  dots <- list(...)
  if (length(dots) == 0L) {
    TRUE
  } else {
    is_fml_dots <- purrr::map_lgl(dots, purrr::is_formula)
    is_fml_list <- length(dots) == 1L &&
      is.list(dots[[1L]]) &&
      all(purrr::map_lgl(dots[[1L]], purrr::is_formula))
    is_fml_dots || is_fml_list
  }
}

is_valid_cond <- function(x) {
  is.null(x) || is_condition(x)
}