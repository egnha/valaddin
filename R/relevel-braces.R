#' Re-level curly braces
#'
#' `relevel_braces()` converts groups of a single curly braces to groups of
#' double curly braces, and vice versa. It assumes, but does not check, that
#' braces are matched.
#'
#' @noRd
#' @param text String.
#' @return String with braces re-leveled, but otherwise unchanged.
#' @examples
#' relevel_braces(".")                         # .
#' relevel_braces("{.}")                       # {{.}}
#' relevel_braces("{{.}}")                     # {.}
#' relevel_braces("{{.}.}")                    # {{{.}.}}
#' relevel_braces("{{.}{.}}")                  # {{{.}{.}}}
#' relevel_braces(".{{.}.}.{{.}}.{.{.}}.{.}")  # .{{{.}.}}.{.}.{{.{.}}}.{{.}}
relevel_braces <- function(text) {
  text_vec <- strsplit(text, NULL)[[1L]]
  paste(relevel_braces_(text_vec), collapse = "")
}

relevel_braces_ <- function(x) {
  ht <- as.integer(cumsum(brace_val(x)))
  rle <- rle(ht != 0L)
  out <- vector("list", length(rle[["values"]]))
  pos <- 0L
  for (i in seq_along(rle[["values"]])) {
    l <- rle[["lengths"]][i]
    seq <- pos + seq_len(l)
    if (rle[["values"]][i]) {
      if (is_double_brace(ht[seq])) {
        out[[i]] <- x[seq][2L:(l - 1L)]
      } else {
        out[[i]] <- c("{", x[seq], "}")
      }
    } else {
      out[[i]] <- x[seq]
    }
    pos <- pos + l
  }
  unlist(out)
}

brace_val <- function(x) {
  val <- numeric(length(x))
  val[x == "{"] <-  1
  val[x == "}"] <- -1
  val
}

is_double_brace <- function(ht) {
  if (length(ht) <= 2L) {
    FALSE
  } else {
    sum(ht == 1L) == 2L
  }
}
