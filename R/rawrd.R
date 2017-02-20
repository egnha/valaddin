# Usage signature of a function (specified by name)
call_chr <- function(nms, ...) {
  stopifnot(is.character(nms))

  call_chr_ <- function(nm) {
    f <- get(nm, ...)
    sig_raw <- deparse(call("function", formals(f), quote(expr = )))
    sig <- paste(trimws(sig_raw, which = "left"), collapse = "")

    sub("^function", nm, trimws(sig, which = "both"))
  }

  vapply(nms, call_chr_, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

# Convert a string-valued function into a vectorized function that joins strings
vectorize_chr <- function(f_chr, join = "\n") {
  force(f_chr)
  force(join)

  function(x)
    paste(vapply(x, f_chr, character(1)), collapse = join)
}

# Make a function that makes raw Rd tags
rd_tag <- function(tag, join, sep) {
  force(join)
  force(sep)
  tag_esc <- paste0("\\", tag, "{")

  function(x) {
    stopifnot(is.character(x))
    paste(tag_esc, paste(x, collapse = join), "}", sep = sep)
  }
}

#' Make a raw Rd tag
#'
#' @param x Character vector of object names.
#' @noRd
#' @name rd_tag
NULL

#' @rdname rd_tag
#' @noRd
#' @param ... Arguments to pass to \code{\link[base]{get}}.
#' @examples
#' rd_usage("ls")
#' rd_usage(c("strictly", "nonstrictly"), pos = "package:valaddin")
rd_usage <- purrr::compose(rd_tag("usage", join = "\n\n", sep = "\n"), call_chr)

#' @rdname rd_tag
#' @noRd
#' @examples
#' rd_alias(c("strictly", "nonstrictly"))
rd_alias <- vectorize_chr(rd_tag("alias", join = "", sep = ""))
