#' Validate objects
#'
#' @param . Object to validate.
#' @param ... Input validation checks.
#' @param error_class Character vector of the error subclass to signal if
#'   validation fails. If `NULL` (the default), the error subclass is
#'   `objectValidationError`.
#'
#' @examples
#' \dontrun{
#' # Assertions valid: data frame returned (invisibly)
#' validate(mtcars,
#'          v_all_map(is.numeric),
#'          v_gt(10)(nrow(.)),
#'          v_contains(c("mpg", "cyl"))(names(.)))
#'
#' # Some assertions invalid: error raised
#' validate(mtcars,
#'          v_all_map(is.numeric),
#'          v_gt(1000)(nrow(.)),
#'          v_contains("cylinders")(names(.)))
#' }
#'
#' @export
validate <- function(., ..., error_class = NULL) {
  validate <- validator(..., error_class = error_class)
  eval(bquote(validate(.(substitute(.)))))
}
#' @rdname validate
#' @export
validator <- vld(UQS(chk_error_class))(
  function(..., error_class = NULL) {
    error_class <- error_class %||% "objectValidationError"
    `_vld`(..., error_class = error_class, env = parent.frame())(pass)
  }
)
# name of argument must coincide with name of validate()'s object-argument
pass <- function(.) invisible(.)

#' Output-validate a function
#'
#' @param f Function.
#' @param ... Input validation checks.
#' @param error_class Character vector of the error subclass to signal if
#'   validation fails. If `NULL` (the default), the error subclass is
#'   `objectValidationError`.
#'
#' @name return_firmly
#' @export
return_firmly <- vld(
  "'f' must be a function" = is.function ~ f,
  UQS(chk_error_class)
)(
  function(f, ..., error_class = NULL) {
    force(f)
    error_class <- error_class %||% "outputValidationError"
    verify <- loosely(validator)(..., error_class = error_class)
    with_sig(
      function() {
        encl <- parent.env(environment())
        encl$verify(eval.parent(`[[<-`(match.call(), 1, encl$f)))
      },
      formals(f),
      attributes(f)
    )
  }
)
