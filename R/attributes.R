#' @include checklist.R
NULL

#' Get attributes of a strict closure
#'
#' A strict closure—a value of the function \code{\link{strictly}()}—stores
#' attributes of the function it "strictifies." The \code{sc_*()} functions
#' extract these attributes:
#' \itemize{
#'   \item \code{sc_core()} gets the original function body.
#'   \item \code{sc_check()} gets the checks, as calls to predicate functions.
#'   \item \code{sc_arg_req()} gets the required arguments, if their absence is
#'     to be checked.
#'   \item \code{sc_logical_void()} gets the interpretation of a
#'     \code{logical(0)} predicate value (which can be either \code{logical(0)},
#'     \code{TRUE}, or \code{FALSE})
#' }
#' \code{is_strict_closure()} checks whether an object is a closure of (S3)
#' class \code{"strict_closure"}.
#'
#' @return \code{sc_core()} returns a language object; \code{sc_check()} returns
#'   a list of checks, where each check is given as a list consisting of a call
#'   (language object) and its string representation; \code{sc_arg_req()}
#'   returns a character vector. \code{NULL} is returned whenever an attribute
#'   is absent.
#' @seealso \code{\link{strictly}}
#' @examples
#' # Probe the attributes of `strictly()` itself, as a strict closure
#'
#' is_strict_closure(strictly)
#'
#' sc_core(strictly)
#' sc_check(strictly)
#' sc_arg_req(strictly)
#' @name sc-attributes
NULL

#' @rdname sc-attributes
#' @export
#' @param x R object.
is_strict_closure <- function(x) {
  purrr::is_function(x) && inherits(x, "strict_closure")
}

get_sc_attr <- function(.attr) {
  force(.attr)
  function(.f) {
    if (!purrr::is_function(.f)) {
      stop("`.f` not an interpreted function", call. = FALSE)
    }
    attr(.f, .attr, exact = TRUE)
  }
}

#' @rdname sc-attributes
#' @export
#' @param .f Interpreted function, i.e., closure (not a primitive function).
sc_core <- get_sc_attr("..sc_core..")

#' @rdname sc-attributes
#' @export
sc_check <- get_sc_attr("..sc_check..")

#' @rdname sc-attributes
#' @export
sc_arg_req <- get_sc_attr("..sc_arg_req..")