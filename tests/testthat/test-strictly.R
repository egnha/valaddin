context("Strictly")

test_that("error raised when .f is not a closure", {
  errmsg <- "`.f` not an interpreted function"
  bad_fns <- list(NULL, NA, log, 1, "A", quote(ls))

  for (f in bad_fns) {
    expect_error(strictly(f), errmsg)
  }
})

test_that("error raised when .warn_missing is not NULL/TRUE/FALSE", {
  f <- function(x) NULL

  # No error if .warn_missing NULL/TRUE/FALSE
  expect_error(strictly(f, .warn_missing = TRUE), NA)
  expect_error(strictly(f, .warn_missing = FALSE), NA)
  expect_error(strictly(f, .warn_missing = NULL), NA)

  # Otherwise, error
  errmsg <- "`.warn_missing` neither NULL nor logical scalar"
  bad_val <- list(NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(strictly(function(x) NULL, .warn_missing = val), errmsg)
  }
})

test_that("error raised when .checklist is an invalid checklist", {
  f <- function(x, y = 1, ...) NULL

  errmsg <- "Invalid argument checks"

  for (chk in invalid_checks) {
    expect_error(strictly(f, chk), errmsg)
    expect_error(strictly(f, .checklist = list(chk)), errmsg)
    expect_error(strictly(f, ~{. > 0}, .checklist = list(chk)), errmsg)
    expect_error(strictly(f, chk, .checklist = list(~{. > 0})), errmsg)
    expect_error(strictly(f, .checklist = list(~{. > 0}, chk)), errmsg)
  }
})

test_that("error raised when .checklist is an non-evaluable checklist", {
  f <- function(x, y = 1, ...) NULL

  for (chk in invalid_checks) {
    expect_error(strictly(f, chk))
    expect_error(strictly(f, .checklist = list(chk)))
    expect_error(strictly(f, ~{. > 0}, .checklist = list(chk)))
    expect_error(strictly(f, chk, .checklist = list(~{. > 0})))
    expect_error(strictly(f, .checklist = list(~{. > 0}, chk)))
  }
})

test_that("function is unchanged when no checks and .warn_missing is NULL", {
  for (args in args_list) {
    f <-  pass_args(args)
    expect_identical(strictly(f), f)
  }
})

test_that("function is unchanged when it has no named arguments", {
  fns <- list(function() NULL, function(...) NULL)
  chks <- list(~is.numeric, "Negative" ~ {. >= 0}, list(~dummy) ~ is.function)

  for (f in fns) {
    expect_identical(strictly(f, .checklist = chks), f)
    expect_identical(strictly(f, .warn_missing = TRUE), f)
    expect_identical(strictly(f, .warn_missing = FALSE), f)
    expect_identical(strictly(f, .checklist = chks, .warn_missing = TRUE), f)
    expect_identical(strictly(f, .checklist = chks, .warn_missing = FALSE), f)
  }
})

test_that("argument signature is preserved", {
  for (args in args_list) {
    f <- make_fnc(args)

    expect_identical(formals(f), as.pairlist(args))
    expect_identical(formals(strictly(f)), formals(f))
  }
})

test_that("original function body/environment/attributes are preserved", {
  set.seed(1)

  len <- sample(length(args_list))

  for (i in seq_along(args_list)) {
    junk <- paste(sample(letters, len[[i]], replace = TRUE), collapse = "")
    body <- substitute(quote(x), list(x = junk))
    attr <- setNames(as.list(sample(LETTERS)), letters)
    env <- new.env(parent = baseenv())
    f <- do.call("structure",
      c(.Data = make_fnc(args_list[[i]], body, env), attr)
    )
    f_strict1 <- strictly(f, list(~x) ~ is.numeric)
    f_strict2 <- strictly(f_strict1, .warn_missing = TRUE)

    # Same body
    nms <- nomen(args_list[[i]])$nm
    args <- setNames(as.list(rep(0, length(nms))), nms)
    expect_identical(do.call(f_strict1, args), junk)
    expect_identical(do.call(f_strict2, args), junk)

    # Same environment
    if (is.null(strict_core(f_strict1))) {
      expect_identical(environment(f_strict1), environment(f))
      expect_identical(environment(f_strict2), environment(f))
    } else {
      expect_identical(environment(strict_core(f_strict1)), environment(f))
      expect_identical(environment(strict_core(f_strict2)), environment(f))
    }

    # Same attributes
    expect_identical(
      attributes(f_strict1)[names(attributes(f_strict1)) != "class"],
      attributes(f)
    )
    expect_identical(
      attributes(f_strict2)[names(attributes(f_strict2)) != "class"],
      attributes(f)
    )
    expect_identical(
      class(f_strict1)[class(f_strict1) != "strict_closure"],
      class(f)
    )
    expect_identical(
      class(f_strict2)[class(f_strict2) != "strict_closure"],
      class(f)
    )
  }
})

test_that("checks in ... are combined with .checklist", {
  sort_checks <- function(..f) {
    dplyr::arrange_(strict_checks(..f), ~string)
  }

  f <- function(x, y = x, z = 0, ...) NULL
  chk1 <- list(~x) ~ {. > 0}
  chk2 <- ~is.numeric

  f_strict <- strictly(f, chk1, chk2)
  calls <- dplyr::arrange_(strict_checks(f_strict), ~string)

  # 4 checks: One global check on 3 arguments, plus a check on 1 argument
  expect_identical(nrow(calls), 4L)

  f_strict2 <- strictly(f, chk1, .checklist = list(chk2))
  expect_identical(sort_checks(f_strict2), calls)

  f_strict3 <- strictly(f, .checklist = list(chk1, chk2))
  expect_identical(sort_checks(f_strict3), calls)
})

test_that("existing checks are preserved when adding new checks", {
  f0 <- function(x, y, ...) NULL
  chks <- list(
    "Not numeric" ~ is.numeric,
    list("y not nonzero" ~ y) ~ {. != 0}
  )
  f <- strictly(f0, .checklist = chks)
  chks_f <- strict_checks(f)

  new_chks <- list(
    "Not less than one" = list("Not less than one" ~ x) ~ {. < 1},
    "Not positive" = "Not positive" ~ {. > 0}
  )

  for (err_msg in names(new_chks)) {
    chk <- new_chks[[err_msg]]

    g <- strictly(f, chk)
    chks_g <- strict_checks(g)

    # Checks of f subset of checks of g
    chks_f_g <- dplyr::distinct(dplyr::bind_rows(chks_g, chks_f))
    expect_identical(
      chks_f_g %>% dplyr::arrange_("string"),
      chks_g %>% dplyr::arrange_("string")
    )

    # All previous checks checked
    expect_error(g("1", 1), "Not numeric: x")
    expect_error(g(1, "1"), "Not numeric: y")
    expect_error(g(1, 0), "y not nonzero")

    # New check checked
    expect_error(g(2, -1), err_msg)
  }
})

test_that(".warn_missing = TRUE adds check for args without default value", {
  f <- function(x, y, z = 0) NULL
  f_warn  <- strictly(f, .warn_missing = TRUE)
  f_check <- strictly(f_warn, list("z not numeric" ~ z) ~ is.numeric)

  # Check applied
  expect_error(f_check(1, 1, "1"), "z not numeric")

  # No warning if all required arguments supplied
  expect_warning(f_warn(1, 1), NA)
  expect_warning(f_check(1, 1), NA)

  args <- list(
    "x, y" = list(),
    "y"    = list(x = 1),
    "x"    = list(y = 1)
  )

  for (nms in names(args)) {
    arg <- args[[nms]]

    expect_warning(out <- do.call(f_warn, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out, do.call(f, arg))

    # Warning behavior persists in presence of checks
    expect_warning(out <- do.call(f_check, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out, do.call(f, arg))
  }
})

test_that(".warn_missing = FALSE removes missing argument check", {
  f <- function(x, y, z = 1, ...) NULL

  # .warn_missing = FALSE no effect if no missing argument checks existant
  expect_warning(f(1), NA)
  expect_identical(strictly(f, .warn_missing = FALSE), f)
  expect_warning(strictly(f, .warn_missing = FALSE)(1), NA)

  f_strict <- strictly(f, .warn_missing = TRUE)

  # .warn_missing = FALSE removes missing argument check if existing
  expect_warning(f_strict(1), "Missing required argument\\(s\\): y")
  expect_identical(strictly(f_strict, .warn_missing = FALSE), f)
  expect_warning(strictly(f_strict, .warn_missing = FALSE)(1), NA)
})

test_that(".warn_missing = NULL preserves missing-argument-check behavior", {
  f <- function(x, y, z = 1, ...) NULL
  fns <- list(
    f,
    strictly(f, ~is.numeric),
    strictly(f, ~is.numeric, .warn_missing = TRUE)
  )

  for (g in fns) {
    expect_identical(strictly(g, .warn_missing = NULL), g)
  }
})

test_that(".warn_missing is ignored when every named arg has default value", {
  f <- function(x = 1, ...) NULL
  f_strict <- strictly(f, ~is.numeric)

  expect_identical(strictly(f, .warn_missing = TRUE), f)
  expect_identical(strictly(f, .warn_missing = FALSE), f)
  expect_identical(strictly(f, .warn_missing = NULL), f)
  expect_identical(strictly(f_strict, .warn_missing = TRUE), f_strict)
  expect_identical(strictly(f_strict, .warn_missing = FALSE), f_strict)
  expect_identical(strictly(f_strict, .warn_missing = NULL), f_strict)
})

test_that(".warn_missing value does not change checks", {
  f0 <- function(x, y, z = 1, ...) x + y + z
  f1 <- strictly(f0,
                 list("x not numeric" ~ x, "y not numeric" ~ y) ~ is.numeric)

  # .warn_missing = FALSE doesn't change checks
  f1_warn_false <- strictly(f1, .warn_missing = FALSE)

  set.seed(1)
  args <- lapply(1:10, function(.) runif(2))
  for (arg in args) {
    out <- f0(arg[[1L]], arg[[2L]])
    # Function evaluated normally if checks pass
    expect_equal(f1(arg[[1L]], arg[[2L]]), out)
    expect_equal(f1_warn_false(arg[[1L]], arg[[2L]]), out)
  }

  nonnumeric <- list(NULL, "A", log, ls, mtcars, quote(ls), TRUE, FALSE, NA)
  for (val in nonnumeric) {
    # Checks performed
    expect_error(f1(x = val, y = 1), "x not numeric")
    expect_error(f1_warn_false(x = val, y = 1), "x not numeric")
    expect_error(f1(y = val, x = 1), "y not numeric")
    expect_error(f1_warn_false(y = val, x = 1), "y not numeric")
  }

  # .warn_missing = TRUE doesn't change checks
  f1_warn_true <- strictly(f1, .warn_missing = TRUE)

  set.seed(1)
  args <- lapply(1:10, function(.) runif(2))
  for (arg in args) {
    out <- f0(arg[[1L]], arg[[2L]])

    expect_equal(f1_warn_true(arg[[1L]], arg[[2L]]), out)
  }

  for (val in nonnumeric) {
    # Checks performed
    expect_error(f1_warn_true(x = val, y = 1), "x not numeric")
    expect_error(f1_warn_true(y = val, x = 1), "y not numeric")
  }

  # Check for missing arguments performed
  expect_warning(purrr::safely(f1_warn_true)(x = 1),
                 "Missing required argument\\(s\\): y")
  expect_warning(purrr::safely(f1_warn_true)(y = 1),
                 "Missing required argument\\(s\\): x")
})

test_that("arguments, whether checked or not, are evaluated lazily", {
  f <- function(x, y) if (x) "True" else y
  msg <- "`y` not an error"
  chk_is_error <-
    list(msg ~ tryCatch({y; FALSE}, error = function(e) TRUE)) ~ isTRUE

  f_strict <- strictly(f, chk_is_error)

  # Verify that y is checked:
  # - Pass
  expect_error(f_strict(TRUE, stop("!")), NA)
  # - Fail
  not_error <- list(TRUE, FALSE, NULL, NA_real_, NaN, letters, mtcars)
  for (. in not_error) expect_error(f_strict(TRUE, .), msg)

  # Verify that y is not forced when function called
  expect_identical(f_strict(TRUE, stop("!")), "True")
})
