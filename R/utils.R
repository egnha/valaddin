#' @importFrom magrittr %>%
`%>%` <- magrittr::`%>%`

#' @importFrom purrr %||%
`%||%` <- purrr::`%||%`

# Clone an environment
clone_env <- lazyeval:::clone_env

#' Determine the names of a pairlist without a default value
#'
#' @param sig Pairlist.
#' @return Character vector.
#' @keywords internal
args_wo_defval <- function(sig) {
  args <- sig[names(sig) != "..."]
  no_defval <- purrr::map_lgl(args, function(.) {
    is.symbol(.) && as.character(.) == ""
  })
  names(args)[no_defval]
}

print_enumerate <- function(x) {
  for (i in seq_along(x)) {
    cat(paste0(i, ") "))
    print(x[[i]])
  }
}