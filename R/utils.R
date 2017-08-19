#' @importFrom nofrills fn as_fn
NULL

`%||%` <- function(x, y) {
  if (rlang::is_empty(x)) y else x
}

all_empty <- function(xs) {
  all(lengths(xs) == 0)
}

`%@%` <- rlang::`%@%`

#' @importFrom rlang UQ UQS UQE ":="
NULL

try_eval_tidy <- function(expr, env = parent.frame()) {
  tryCatch(
    rlang::eval_tidy(expr, env = env),
    error = identity
  )
}

make_as <- function(this) {
  is_this <- paste("is", this, sep = "_")
  function(x)
    `attr<-`(x, is_this, TRUE)
}
check_is <- function(this) {
  is_this <- paste("is", this, sep = "_")
  function(x)
    isTRUE(attr(x, is_this, exact = TRUE))
}
check_is_caller <- function(nm) {
  caller <- as.name(nm)
  function(x)
    is.call(x) && identical(x[[1]], caller)
}
check_is_class <- function(cls) {
  force(cls)
  function(x)
    inherits(x, cls)
}

# Substitute string into call, to avoid making a binding that could take
# precedence over those in higher environments
glue_text <- function(text, env, data = NULL, ...) {
  eval(bquote(glue::glue_data(.x = data, .(text), .envir = env, ...)))
}

deparse_collapse <- function(x) {
  d <- deparse(x)
  if (length(d) > 1)
    paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  else
    d
}

enumerate_many <- function(x, many = 2) {
  if (length(x) >= many)
    paste(
      vapply(seq_along(x), function(i) sprintf("%d) %s\n", i, x[[i]]),
             character(1)),
      collapse = ""
    )
  else
    paste0(x, "\n")
}

comma_collapse <- function(x, width = 60) {
  x_str <- paste(x, collapse = ", ")
  if (nchar(x_str) > width)
    sprintf("%s ... [TRUNCATED]", substr(x_str, 1, width))
  else
    x_str
}

partial <- function(f, arg_fill) {
  if (rlang::is_empty(arg_fill))
    return(f)
  f <- rlang::as_closure(f)
  fill_args <- function() {
    arg_call <- rlang::node_cdr(sys.call(-1))
    as.call(c(f, arg_fill, arg_call))
  }
  function(...) {
    call <- fill_args()
    rlang::eval_bare(call, parent.frame())
  }
}
