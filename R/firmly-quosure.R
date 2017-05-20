assemble_checks <- function(chk, nm, symb) {}

validate <- function(f, chks, sig, nm) {}
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
