#' @include strictly.R
NULL

remove_check_ <- function(..f, which) {
  calls <- sc_check(..f)
  new_calls <- if (is.logical(which)) {
    calls[!which]
  } else {
    calls[setdiff(seq_along(calls), as.integer(which))]
  }
  sig <- formals(..f)
  strict_closure(
    sig          = sig,
    arg_symb     = nomen(sig)$symb,
    body         = sc_core(..f),
    env          = environment(..f),
    attr         = attributes(..f),
    class        = class(..f),
    calls        = new_calls,
    arg_req      = sc_arg_req(..f),
    logical_void = sc_logical_void(..f)
  )
}

#' Remove a check from a strict closure
#'
#' @param ..f Strict closure, i.e., function of class \code{"strict_closure"}.
#' @param which Logical or numeric vector by which to subset
#'   \code{sc_check(.f)}.
#' @return Strict closure.
#' @export
remove_check <- strictly_(
  remove_check_,
  list("`..f` not a strict closure" ~ ..f) ~
    is_strict_closure,
  list("`which` not logical or numeric" ~ which) ~
  {is.logical(.) || is.numeric(.)},
  list("Range of `which` not compatible with checks of ..f" ~
         list(which, length(sc_check(..f)))) ~
    purrr::lift(is_subset_vec),
  .warn_missing = TRUE
)