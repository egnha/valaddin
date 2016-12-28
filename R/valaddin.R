#' valaddin — Functional Input Validation
#'
#' The valaddin package provides a functional operator, \code{strictly()}, that
#' augments functions with input validation. You supply a function \code{f}
#' along with some input checks, and \code{strictly()} returns a function that
#' applies \code{f} \emph{strictly}: before \code{f} is called, its inputs are
#' checked, and precise errors are generated for failing checks.
#' \cr\cr
#' Checks are concisely expressed using formulas, \code{q ~ p}, where the
#' right-hand side \code{p} is the "check"—a predicate function—and the
#' left-hand side \code{q} is the "check item"—items for \code{p} to check,
#' along with (optional) custom failure messages. Additionally, abbreviations
#' are provided to express common use cases, such as one-sided formulas to
#' specify a uniform check to all arguments. The example below shows how to add
#' type validation to all arguments (they must be numeric), while additionally
#' enforcing a relation between them (they must be positive and appear in
#' increasing order).
#' \cr\cr
#' Using \code{strictly()} to add input validation to your functions improves
#' the legibility, reusability, and reliability of your code:
#' \itemize{
#'   \item Emphasize the core logic of your functions by excising validation
#'   boilerplate code.
#'   \item Reduce repetition by reusing common checks across functions with
#'   similar input requirements.
#'   \item Vary the strictness of a function as needed, either on-the-fly, for
#'   use in magrittr \code{\link[magrittr]{\%>\%}}-lines, or when entering
#'   "hazardous" environments (e.g., if your function is being fed data from a
#'   potentially unreliable source).
#'   \item Ensure that your function gets what it needs, with minimal fuss; and
#'   when it doesn't, immediately pinpoint the deficiency—did a check, or its
#'   very evaluation, fail?
#' }
#' More information can be found in the documentation page,
#' \link{strictly}.
#'
#' @examples
#' library(valaddin)
#'
#' f <- function(x, y) log(x - y)
#' f_strict <- strictly(f, ~is.numeric,
#'                      list(~y, "`x` not greater than `y`" ~ x - y) ~ {. > 0})
#'
#' f_strict(4, 2)   # 0.6931472
#' f_strict(3, 0)   # Error: FALSE: (purrr::as_function(~{. > 0}))(y)
#' f_strict("4", 2) # Error: FALSE: is.numeric(x), error evaluating check
#' @docType package
#' @name valaddin
NULL