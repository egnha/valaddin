# Call signature of a function (specified by name)
call_sig <- function(x, width) {
  stopifnot(is.character(x))
  is_op <- grepl("^%.+%$", x)
  x[ is_op] <- vapply(x[ is_op], call_sig_op, character(1), USE.NAMES = FALSE)
  x[!is_op] <- vapply(x[!is_op], call_sig_fn, character(1), USE.NAMES = FALSE,
                      width = width)
  x
}

call_sig_fn <- function(nm, width) {
  # Allowed range of values for width.cutoff parameter of deparse()
  stopifnot(width >= 20L, width <= 500L)

  sig <- formals(get(nm, mode = "function"))
  expr <- deparse(call("function", sig, quote(expr = ))) %>%
    paste(collapse = "") %>%
    sub("^function", nm, .) %>%
    {parse(text = ., keep.source = FALSE)[[1L]]}
  indent <- paste(rep(" ", nchar(nm)), collapse = "")

  paste(deparse_lines(expr, indent, width), collapse = "\n")
}

# The inaptly named "width.cutoff" of deparse() is a _lower_ bound for lengths
deparse_lines <- function(expr, indent, width) {
  w <- width
  exceed_width <- TRUE
  while (exceed_width) {
    x <- deparse_reindent(expr, indent, w)
    exceed_width <- any(vapply(x, nchar, integer(1)) > width)
    w <- w - 1L
  }
  x
}
deparse_reindent <- function(expr, indent, width) {
  expr %>%
    deparse(width.cutoff = width) %>%
    trimws(which = "both") %>%
    {`[<-`(., -1L, value = paste(indent, .[-1L]))}
}

call_sig_op <- function(nm) {
  op <- get(nm, mode = "function")
  args <- names(formals(op))
  nm_esc_pct <- gsub("%", "\\\\%", nm)
  paste(args[1L], nm_esc_pct, args[2L])
}

# Convert a string-valued function into a vectorized function that joins strings
vec_strjoin <- function(f_str, join = "\n") {
  force(f_str)
  force(join)

  function(x)
    paste(vapply(x, f_str, character(1)), collapse = join)
}

# Make a function that makes raw Rd markup
rd_markup <- function(cmd, join = "", sep = "") {
  force(join)
  force(sep)
  cmd_opening <- paste0("\\", cmd, "{")

  function(x) {
    stopifnot(is.character(x))
    paste(cmd_opening, paste(x, collapse = join), "}", sep = sep)
  }
}

#' Make raw Rd markup
#'
#' @param x Character vector of object names.
#' @name rd_markup
#' @noRd
NULL

#' @rdname rd_markup
#' @examples
#' rd_alias(c("firmly", "loosely"))
#' @noRd
rd_alias <- vec_strjoin(rd_markup("alias"))

#' @rdname rd_markup
#' @param width Width cutoff attempt, cf. \code{\link{deparse}}.
#' @examples
#' rd_usage("ls")
#' rd_usage(c("firmly", "loosely"), pos = "package:valaddin")
#' @noRd
rd_usage <- function(x, width = 80L) {
  rd_markup("usage", join = "\n\n", sep = "\n")(call_sig(x, width))
}

#' @rdname rd_markup
#' @examples
#' rd_seealso("Logarithm: \\code{\\link{log}}, \\code{\\link[utils]{head}}")
#' @noRd
rd_seealso <- rd_markup("seealso", join = "\n\n", sep = "\n")
