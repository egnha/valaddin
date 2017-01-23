#' @include utils.R
NULL

#' Deparse a language object as a single string
#'
#' @param x Language object.
#' @return String.
#' @details Used in validation check template, must therefore be exported.
#' @keywords internal
deparse_collapse <- function(x) {
  paste(trimws(deparse(x), which = "both"), collapse = "")
}

is_anon_function <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("function"))
}

string_funexpr <- function(expr) {
  if (is_anon_function(expr)) {
    chr <- deparse(expr)
    chr_body <- head(chr[-1L], -1L) %>%
      trimws(which = "both") %>%
      paste(collapse = "; ")
    chr_fun <- paste0("(", chr[[1L]], chr_body, tail(chr, 1L), ")")
  } else {
    chr_fun <- deparse_collapse(expr)
  }

  paste0(chr_fun, "(%s)")
}