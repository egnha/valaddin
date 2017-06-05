#' Validate objects
#'
#' @name validate
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Valid assertions: data frame returned (invisibly)
#' mtcars %>%
#'   validate(
#'     vld_all(~sapply(., is.numeric)),
#'     ~{nrow(.) > 10},
#'     vld_all(~c("mpg", "cyl") %in% names(.))
#'   )
#'
#' # Invalid assertions: error raised
#' mtcars %>%
#'   validate(
#'     vld_all(~sapply(., is.numeric)),
#'     ~{nrow(.) > 1000},
#'     vld_all(~c("mpg", "cylinders") %in% names(.))
#'   )
#' }
NULL

validator <- function(..., .checklist = list(), .error_class = character()) {
  firmly_(function(.) invisible(.),
          ..., .checklist = .checklist, .error_class = .error_class)
}

#' @rdname validate
#' @param . Object to validate.
#' @param \dots Input-validation check formula(e).
#' @param .checklist List of check formulae. (These are combined with check
#'   formulae provided via \code{\dots}.)
#' @param .error_class Subclass of the error condition to be raised when an
#'   input validation error occurs (character).
#' @export
validate <- function(., ..., .checklist = list(),
                     .error_class = "validationError") {
  # Assign validator to `validate` to get appropriate call in error message
  validate <-
    validator(..., .checklist = .checklist, .error_class = .error_class)
  validate(.)
}

#' @rdname validate
#' @param .f Interpreted function, i.e., closure.
#' @param .checks List of check formulae, optionally containing a character
#'   vector named \code{.error_class}, corresponding to the similarly named
#'   argument.
#' @export
`%checkout%` <- function(.f, .checks) {
  force(.f)

  validate_out <- local({
    nms <- names(.checks) %||% character(length(.checks))
    chk <- .checks[nms != ".error_class"]
    err <- .checks[[".error_class"]] %||% character()
    validator(.checklist = chk, .error_class = err)
  })
  `formals<-`(
    function () {
      encl <- parent.env(environment())
      encl$validate_out(eval.parent(`[[<-`(match.call(), 1L, encl$.f)))
    },
    value = formals(.f)
  )
}
