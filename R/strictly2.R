#' @include validate.R
NULL

project_value <- function(.em) {
  if (is.null(.em$error)) {
    .em$value
  } else {
    msg <- if (is.data.frame(.em$error)) {
      enumerate_many(.em$error$msg)
    } else {
      .em$error
    }

    stop(msg, call. = FALSE)
  }
}

strictly2_ <- strictly_with(project_value)
safely_    <- strictly_with(identity, .fn_type = error_function)

checks <- list(
  list("`.f` not an interpreted function" ~ .f) ~
    purrr::is_function,
  list("`.warn_missing` not a logical scalar" ~ .warn_missing) ~
    {purrr::is_scalar_logical(.) && !purrr::is_empty(.)}
)

#' @export
strictly2 <- strictly2_(strictly2_, .checklist = checks, .warn_missing = TRUE)

#' @export
safely <- strictly2_(safely_, .checklist = checks, .warn_missing = TRUE)
