#' Clone an environment
#'
#' @details Taken from the section
#'   \href{http://adv-r.had.co.nz/dsl.html#latex}{LaTeX} of the book
#'   \emph{Advanced R}, by Hadley Wickham.
#' @param env,parent Environments.
#' @return Clone of the environment \code{x}.
#' @keywords internal
clone_env <- function(env, parent = parent.env(env)) {
  list2env(as.list(env, all.names = TRUE), parent = parent)
}

#' Default value for \code{NULL}.
#'
#' @details Taken from the section
#'   \href{http://adv-r.had.co.nz/Functions.html#special-calls}{Special calls}
#'   of the book \emph{Advanced R}, by Hadley Wickham.
#' @param x,y R objects.
#' @name null-default
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create a condition object
#'
#' @details Taken from the section
#'   \href{http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling}{Condition
#'    handling} of the book \emph{Advanced R}, by Hadley Wickham.
#' @param subclass Subclass of \code{"condition"} (string).
#' @param message Message (string).
#' @param call Call object.
#' @param ... Additional arguments to pass to
#' @return Condition object (class \code{"condition"}).
#' @keywords internal
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    list(message = message, call = call),
    class = c(subclass, "condition"),
    ...
  )
}
is_condition <- function(x) {
  inherits(x, "condition")
}

#' Determine the names of a pairlist without a default value
#'
#' @param sig Pairlist.
#' @return Character vector.
#' @keywords internal
args_wo_defval <- function(sig) {
  is_symb_void <- function(.) is.symbol(.) && as.character(.) == ""
  args <- sig[names(sig) != "..."]
  no_defval <- purrr::map_lgl(args, is_symb_void)
  names(args)[no_defval]
}

#' Unpack a list of a list
#'
#' @param x List.
#' @return \code{x[[1L]]}, if \code{x} is a list of a list, otherwise \code{x}.
#' @keywords internal
unpack <- function(x) {
  if (length(x) == 1L && is.list(x[[1L]])) x[[1L]] else x
}

print_enumerate <- function(x) {
  for (i in seq_along(x)) {
    cat(paste0(i, ") "))
    print(x[[i]])
  }
}