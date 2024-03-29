#' valaddin: Functional Input Validation
#'
#' \emph{valaddin} provides a functional operator, \code{\link{firmly}}, that
#' enhances functions with input validation. You supply a function \code{f}
#' along with input validation requirements, and \code{firmly} returns a
#' function that applies \code{f} \dQuote{firmly}: before a call to \code{f} is
#' attempted, its inputs are checked, and if any check fails, an error halts
#' further execution with a message tabulating every failing check. Because
#' \code{firmly} implements input validation by operating on whole functions
#' rather than values, it is suitable for both programming and interactive use.
#' \cr\cr
#' Using \code{firmly} to add input validation to your functions improves the
#' legibility, reusability, and reliability of your code:
#' \itemize{
#'   \item Emphasize the core logic of your functions by excising validation
#'     boilerplate.
#'   \item Reduce duplication by reusing common checks across functions with
#'     common input requirements.
#'   \item Make function outputs more predictable by constraining their inputs.
#'   \item Vary the strictness of a function according to need and circumstance.
#' }
#'
#' @details For an example-oriented overview of valaddin, see
#'   \code{vignette("valaddin")}.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
