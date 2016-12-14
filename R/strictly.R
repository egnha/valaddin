strictly <- function(f, ..., cond = NULL) {
  ..cond <- cond %||% identity
  ..chks <- list(...)

  is_fml <- purrr::map_lgl(..chks, is_formula)
  if (!(purrr::is_function(f) && all(is_fml))) {
    stop("Invalid arguments for strictly()", call. = FALSE)
  }

  # Use `..` to make clashes with names in body(.f) improbable
  body <- substitute({
    ..env.. <- as.list(environment(), all.names = TRUE)
    ..is_ok.. <- purrr::map_lgl(chks, lazyeval::f_eval_lhs, data = ..env..)
    if (!all(..is_ok..)) {
      ..msg.. <- chks[!..is_ok..] %>%
        purrr::map_chr(~ as.character(lazyeval::f_eval_rhs(.x, ..env..))) %>%
        paste(collapse = "; ")
      stop(..cond(..msg..))
    }
    .body
  }, list(.body = body(f), chks = ..chks))

  f_stc <- eval(call("function", formals(f), as.call(body)))
  environment(f_stc) <- list2env(as.list(environment(f), all.names = TRUE),
                                 parent = parent.env(environment(f)))
  environment(f_stc)[c("..cond", "..chks")] <- list(..cond, ..chks)

  f_stc
}