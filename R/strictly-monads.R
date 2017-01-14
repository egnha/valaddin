#' @include strictly.R monads.R
NULL

eval_expr <- function(.expr) {
  force(.expr)

  function(.env, ...) suppressWarnings(eval(.expr, envir = .env, ...))
}

pred_vals <- dplyr::data_frame(
  desc = c("NULL", "NA", "void", "not logical", "TRUE", "FALSE"),
  pred = list(
    purrr::is_null,
    function(.) purrr::is_vector(.) && is.na(.),
    function(.) identical(., logical(0)),
    Negate(purrr::is_scalar_logical),
    isTRUE,
    function(.) identical(., FALSE)
  ),
  value = c(rep(NA, 4), TRUE, FALSE)
)
pred_vals$desc <- sprintf("Predicate value is %s: %%s", pred_vals$desc)

# .msg must not contain "%s"
value_as_msg <- function(.em, .expr, .string, .msg, .vals = pred_vals) {
  if (is.null(.em$error)) {
    .vals$desc[length(.vals$desc)] <- .msg
    wh <- which_first_true(.vals$pred, .em$value)
    msg   <- sprintf(.vals$desc[wh], .string)
    value <- .vals$value[wh]
  } else {
    msg   <- sprintf("Error evaluating check `%s`: %s", .string, .em$error)
    value <- NA
  }

  dplyr::data_frame(
    expr   = list(.expr),
    string = .string,
    msg    = msg,
    value  = value
  )
}

eval_check <- function(.expr, .string, .msg) {
  force(.expr); force(.string); force(.msg)

  writer_function(
    function(.env) {
      eval_quietly <- function(.) suppressWarnings(eval(., .env))
      res <- error_fmap(eval_quietly)(error_unit(.expr))
      log <- value_as_msg(res, .expr, .string, .msg)

      writer(value = .env, log = log)
    }
  )
}

checker <- function(.expr, .string, .msg) {
  f <- eval_check(.expr, .string, .msg)

  function(.wm) bind.writer_monad(.wm, f)
}

validate_checks <- function(.wm) {
  if (is.null(.wm$log)) {
    x <- list(value = .wm$value, error = NULL)
  } else {
    errors <- dplyr::bind_rows(.wm$log) %>%
      `[`(is.na(.$value) | .$value == FALSE, )

    x <- if (nrow(errors)) {
      list(value = NULL, error = errors)
    } else {
      list(value = .wm$value, error = NULL)
    }
  }

  error(x)
}

warn <- function(.ref_args) {
  force(.ref_args)

  function(.call) {
     missing <- setdiff(.ref_args, names(.call[-1L]))

     if (length(missing)) {
       msg <- missing %>%
         paste(collapse = ", ") %>%
         sprintf("Missing required argument(s): %s", .)
       warning(msg, call. = FALSE)
     }

     invisible(.call)
  }
}

proto_strictly_mnd <- function(.f, ..., .checklist = list(),
                              .warn_missing = FALSE, .process) {
  force(.process)

  chks <- c(list(...), .checklist)

  if (!is_checklist(chks)) {
    stop("Invalid argument checks", call. = FALSE)
  }

  sig <- formals(.f)
  arg <- nomen(sig)
  check_inputs <- if (!length(chks)) {
    NULL
  } else {
    calls <- lapply(chks, assemble, .nm = arg$nm, .symb = arg$symb) %>%
      dplyr::bind_rows()
    purrr::pmap(unname(calls), checker)
  }
  validate <- c(writer_unit, check_inputs, validate_checks)
  call_with <- caller(.f)
  maybe_join <- if (is_error_function(.f)) join.error_monad else identity
  maybe_warn <- if (.warn_missing) warn(arg$nm[arg$wo_value]) else invisible

  ..f <- function() {
    call <- match.call()

    maybe_warn(call)

    env <- do.call(lazyeval::lazy_dots, arg$symb) %>%
      lazy_assign(env = new.env(parent = parent.frame()))
    call_fn <- call_with(call)
    pipeline <- c(validate, error_fmap(eval_expr(call_fn)), maybe_join)

    .process(magrittr::freduce(env, pipeline))
  }

  with_sig(..f, sig)
}

strictly_with <- function(.process, .fn_type = NULL) {
  force(.process)

  type <- .fn_type %||% identity

  function(.f, ..., .checklist = list(), .warn_missing = FALSE) {
    type(
      proto_strictly_mnd(.f, ..., .checklist = .checklist,
                     .warn_missing = .warn_missing, .process = .process)
    )
  }
}

project_value <- function(.em) {
  if (is.null(.em$error)) {
    .em$value
  } else {
    msg <- if (is.data.frame(.em$error)) {
      enumerate_many(.em$error$msg)
    } else {
      .em$error
    }

    stop(msg, call. = FALSE)
  }
}

strictly_mnd_ <- strictly_with(project_value)
safely_mnd_    <- strictly_with(identity, .fn_type = error_function)

checks <- list(
  list("`.f` not an interpreted function" ~ .f) ~
    purrr::is_function,
  list("`.warn_missing` not a logical scalar" ~ .warn_missing) ~
  {purrr::is_scalar_logical(.) && !purrr::is_empty(.)}
)

#' @export
strictly_mnd <- strictly_mnd_(strictly_mnd_, .checklist = checks,
                              .warn_missing = TRUE)

#' @export
safely_mnd <- strictly_mnd_(safely_mnd_, .checklist = checks,
                            .warn_missing = TRUE)
