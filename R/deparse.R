# Deparse a language object as a single string
deparse_collapse <- function(x) {
  paste(trimws(deparse(x), which = "both"), collapse = "")
}

is_anon_function <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("function"))
}

string_funexpr <- function(expr) {
  if (is_anon_function(expr)) {
    ch <- deparse(expr)
    ch_body <- paste(
      trimws(utils::head(ch[-1L], -1L), which = "both"),
      collapse = "; "
    )
    ch_fun <- paste0("(", ch[[1L]], ch_body, utils::tail(ch, 1L), ")")
  } else {
    ch_fun <- deparse_collapse(expr)
  }

  ch_fun
}