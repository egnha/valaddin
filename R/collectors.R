new_vld_collector <- function(f) {
  collect <- collect_with(f)
  function(...) {
    dd <- dots_definitions(...)
    if (all(lengths(dd) == 0))
      return(NULL)
    collect(dd$dots, dd$defs)
  }
}

collect_with <- function(f) {
  collect_unnamed <- collect_unnamed_with(f)
  collect_named <- collect_named_with(f)
  function(dots, defs)
    c(lapply(dots, collect_unnamed), lapply(defs, collect_named))
}

collect_unnamed_with <- function(f) {
  force(f)
  function(x) {
    expr <- f_rhs(x)
    if (is.list(expr))
      expr
    else
      set_empty_msg(f(unpack_quosure(x)))
  }
}
set_empty_msg <- function(x) {
  list(msg = empty_msg, chk = x)
}
is_empty_msg <- function(msg) {
  !nzchar(f_rhs(msg))
}
empty_msg <- quo("")

collect_named_with <- function(f) {
  force(f)
  nms_check <- c(lhs = "msg", rhs = "chk")
  function(x) {
    x <- lapply(x, unpack_quosure)
    x$rhs <- f(x$rhs)
    names(x) <- nms_check[names(x)]
    x
  }
}

unpack_quosure <- function(x) {
  rhs <- f_rhs(x)
  if (is_quosure(rhs)) rhs else x
}

as_call <- function(x) {
  expr <- f_rhs(x)
  if (is_bare_head(expr))
    f_rhs(x) <- new_language(expr)
  x
}
is_bare_head <- function(x) {
  !is.call(x) ||
    is_block(x) || is_fn_declaration(x) || is_ns_public(x) || is_ns_private(x)
}
check_is_symb <- function(nm) {
  symb <- as.symbol(nm)
  function(x)
    identical(x[[1]], symb)
}
is_block          <- check_is_symb("{")
is_fn_declaration <- check_is_symb("function")
is_ns_public      <- check_is_symb("::")
is_ns_private     <- check_is_symb(":::")

#' Specify validation checks
#'
#' @param ... Checks or expressions to check.
#'
#' @export
vld_checks <- new_vld_collector(as_call)

#' @export
#' @rdname vld_checks
vld_exprs <- new_vld_collector(identity)
