#' @importFrom magrittr %>%
`%>%` <- magrittr::`%>%`

#' @importFrom purrr %||%
`%||%` <- purrr::`%||%`

# Clone an environment
clone_env <- lazyeval:::clone_env

#' @export
deparse_collapse <- function(x) {
  paste(trimws(deparse(x), which = "both"), collapse = "")
}

#' @export
enumerate_many <- function(x, many = 1L) {
  if (length(x) > many) {
    paste(
      purrr::map_chr(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]])),
      collapse = ""
    )
  } else {
    x
  }
}