#' valaddin — Validate Function Inputs
#'
#' The valaddin package provides a functional operator, \code{firmly()}, that
#' augments functions with input validation. You supply a function \code{f}
#' along with some input checks, and \code{firmly()} returns a function that
#' applies \code{f} \emph{firmly}: before \code{f} is called, its inputs are
#' checked, and precise errors are generated for failing checks.
#'
#' Checks are concisely expressed using formulas, \code{q ~ p}, where the
#' right-hand side \code{p} is the \dQuote{check}—a predicate function—and the
#' left-hand side \code{q} is the \dQuote{check item}—items for \code{p} to
#' check (which may encode custom error messages). Additionally, abbreviations
#' are provided to express common use cases, such as one-sided formulas to
#' specify a uniform check to all arguments. The example below shows how to add
#' type validation to all arguments (they must be numeric), while additionally
#' enforcing a relation between them (they must be positive and appear in
#' increasing order).
#'
#' Using \code{firmly()} to add input validation to your functions improves the
#' legibility, reusability, and reliability of your code:
#' \itemize{
#'   \item Emphasize the core logic of your functions by excising validation
#'     boilerplate code.
#'   \item Reduce repetition by reusing common checks across functions with
#'     similar input requirements.
#'   \item Vary the rigidity of a function as needed, either on-the-fly, for use
#'     in
#'     \href{https://cran.r-project.org/package=magrittr}{\pkg{magrittr}}
#'     \code{\link[magrittr]{\%>\%}}-lines, or when entering \dQuote{hazardous}
#'     environments (e.g., if your function is being fed data from a potentially
#'     unreliable source).
#'   \item Ensure that your function gets what it needs, with minimal fuss; and
#'     when it doesn't, immediately pinpoint the deficiency—did a check, or its
#'     very evaluation, fail?
#' }
#' More information can be found in the documentation page \link{firmly}.
#'
#' @examples
#' \dontrun{
#'
#' library(valaddin)
#'
#' secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
#' secant_firm <- firmly(secant, list(~f) ~ is.function,
#'                               list(~x, ~dx) ~ is.numeric)
#' secant_firmer <- firmly(secant_firm, list("`dx` not non-zero" ~ dx) ~
#'                                        {abs(.) > .Machine$double.eps ^ 0.5})
#'
#' secant_firm(log, 1, .1)          # 0.9531018
#' secant_firm("log", 1, .1)        # Error: "FALSE: is.function(f)"
#' secant_firm(log, "1", .1)        # Error: "FALSE: is.numeric(x)"
#' secant_firm(log, 1, log(1))      # NaN
#' secant_firmer(log, 1, log(1))    # Error: "`dx` not non-zero"
#' secant_firmer(log, 1, log("1"))  # Error: Error evaluating checks
#' }
#'
#' @aliases NULL
#' @docType package
#' @name valaddin
NULL
