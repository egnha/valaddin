q_deparse_call <- function(q, args) {
  rlang::quo_text(rlang::expr(UQE(q)(UQ(args))))
}

default_msg <- function(q, exprs, default) {
  if (nzchar(default)) {
    text <- vapply(exprs, rlang::quo_text, character(1))
    paste(default, encodeString(text, quote = "`"), sep = ": ")
  } else {
    text <- vapply(exprs, q_deparse_call, character(1), q = q)
    ws <- ifelse(grepl("\n", text), "\n", " ")
    paste0("FALSE:", ws, text)
  }
}

validation_df <- function(q, exprs, msgs) {
  n <- length(exprs)
  d <- list(
    pred = `[<-`(vector("list", n), list(q)),
    expr = exprs,
    msg  = msgs
  )
  class(d) <- "data.frame"
  attr(d, "row.names") <- .set_row_names(n)
  d
}

parse_check <- function(quo_chk, nm_arg, sym_arg) {
  q <- lambda(quo_chk[[1]])
  if (length(quo_chk) == 1) {
    exprs <- lapply(sym_arg, rlang::new_quosure, env = emptyenv())
  } else {
    exprs <- quo_chk[-1]
  }
  msgs <- names(exprs) %||% character(length(exprs))
  not_named <- !nzchar(msgs)
  msgs[not_named] <- default_msg(q, exprs[not_named], names(quo_chk)[1])
  validation_df(q, exprs, msgs)
}

safely_name <- function(exprs, ..., fill) {
  nms <- list(...)
  n <- max(unlist(lapply(exprs, function(e) rapply(as.list(e), nchar))))
  setNames(
    paste(nms, paste(character(n), collapse = fill), sep = fill),
    nms
  )
}

vld_ <- function(..., checklist = NULL) {
  quo_chks <- c(rlang::quos(...), checklist)
  function(f) {
    sig <- formals(f)
    arg <- nomen(sig)
    chks <- lapply(quo_chks, assemble_checks, nm = arg$nm, symb = arg$symb)
    validate(f, chks, sig, arg$nm)
  }
}

firmly_q <- function(f, ..., checklist = NULL) {
  vld_(..., checklist = checklist)(f)
}

# msg <- "Not positive"
# a <- 1
# quos(
#   is.numeric,
#   quos(is.numeric, x, y),
#   !!msg := quos({. > 0}, x, !!local_msg := y - !!a),
#   "Error message" = quos(is.character, x, y, paste(z, !!a))
# )
