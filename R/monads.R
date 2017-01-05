nm_typed_obj <- function(type, object) {
  paste(type, object, sep = "_")
}
monad_class <- function(type) {
  nm_typed_obj(type, "monad")
}
monad_function_class <- function(type) {
  nm_typed_obj(monad_class(type), "function")
}

monad_type <- function(type, data, metadata) {
  cls <- monad_class(type)
  nms <- c(data, metadata)

  function(x, ...) {
    obj <- if (missing(x)) list(...) else x

    stopifnot(is.list(obj) && all(nms %in% names(obj)))

    structure(obj, class = c(cls, class(obj[[data]])))
  }
}
monad_function <- function(type, ...) {
  cls <- monad_function_class(type)
  function(f) structure(f, class = c(cls, class(f)))
}

is_it <- function(this) {
  force(this)
  function(x) inherits(x, this)
}
is_monad_type <- function(type, ...) is_it(monad_class(type))
is_monad_function <- function(type, ...) is_it(monad_function_class(type))

monad_unit <- function(type, data, metadata) {
  cls <- monad_class(type)
  nms <- c(data, metadata)

  function(x) {
    obj <- setNames(list(x, NULL), nms)
    structure(obj, class = c(cls, class(x)))
  }
}

#' Provide ingredients for making a monad
#'
#' A monad of augmented data records ordinary data (e.g., a numerical value),
#' together with metadata (e.g., a log message). \code{setup_monad()} sets up
#' the ingredients for making a monad, named \code{<type>} (string), of
#' augmented data:
#' \itemize{
#'   \item \code{<type>()}: Creates a monadic object from an ordinary one.
#'   \item \code{<type>_function()}: Designates a function as one with monadic
#'     output.
#'   \item \code{is_<type>()}: Is an object a monad?
#'   \item \code{is_<type>_function()}: Does a function have monadic output?
#'   \item \code{<type>_unit()}: Unit for monad.
#' }
#' (To finish monad setup, one needs to provide a "bind" method for the generic
#' function \code{bind()}.)
#'
#' @param type Name of monad type (string).
#' @param data,metadata Name of data, resp. metadata, fields of monadic object
#'   (string).
#' @param env Environment to which to assign monad-structure functions.
#' @return Invisibly returns arguments.
#' @keywords internal
#' @examples
#' setup_monad("writer", data = "value", metadata = "log")
#' writer(value = 3, log = "Three")
#' writer_unit(3)
setup_monad <- function(type, data, metadata, env = parent.frame()) {
  fns <- list(
    "%s"             = monad_type,
    "%s_function"    = monad_function,
    "is_%s"          = is_monad_type,
    "is_%s_function" = is_monad_function,
    "%s_unit"        = monad_unit
  )
  names(fns) <- sprintf(names(fns), type)

  for (nm in names(fns)) {
    assign(nm, fns[[nm]](type, data, metadata), envir = env)
  }

  invisible(c(type, data, metadata))
}

#' @export
bind <- function(m, ...) {
  UseMethod("bind")
}

#' @export
join <- function(m, ...) {
  UseMethod("join")
}

not_monadic <- function(m, ...) {
  arg <- deparse(substitute(x, list(x = m)), control = "delayPromises")
  stop("Not of monadic type: ", arg, call. = FALSE)
}

#' @export
bind.default <- not_monadic

#' @export
join.default <- not_monadic

fmap <- function(type, env = parent.frame()) {
  monadic <- function(obj) get(nm_typed_obj(type, obj), envir = env)

  unit <- monadic("unit")
  monadically <- monadic("function")

  function(f) {
    stopifnot(is.function(f))

    monadically(
      function(m) bind(m, monadically(purrr::compose(unit, f)))
    )
  }
}

# Error monad -------------------------------------------------------------

setup_monad("error", data = "value", metadata = "error")

#' @export
bind.error_monad <- function(m, f) {
  stopifnot(is_error_function(f))

  m_out <- if (!is.null(m$error)) {
    list(value = NULL, error = m$error)
  } else {
    tryCatch(
      f(m$value),
      error = function(e) list(value = NULL, error = e$message)
    )
  }
  error(m_out)
}

error_fmap <- fmap("error")

#' @export
join.error_monad <- function(m) {
  stopifnot(is.null(m$value) || is_error(m$value))

  if (is.null(m$error)) {
    error(value = m$value$value, error = m$value$error)
  } else {
    error(value = NULL, error = m$error)
  }
}

# Writer monad ------------------------------------------------------------

setup_monad("writer", data = "value", metadata = "log")

encapsulate <- function(x) if (is.null(x)) NULL else list(x)

#' @export
bind.writer_monad <- function(m, f) {
  stopifnot(is_writer_function(f))

  out <- f(m$value)
  m_out <- list(
    value = out$value,
    log   = c(m$log, encapsulate(out$log))
  )
  writer(m_out)
}

writer_fmap <- fmap("writer")

#' @export
join.writer_monad <- function(m) {
  stopifnot(is.null(m$value) || is_writer(m$value))

  writer(value = m$value$value, log = c(m$log, encapsulate(m$value$log)))
}