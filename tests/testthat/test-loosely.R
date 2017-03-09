context("Loosely")

fs <- lapply(args_list, pass_args)

test_that("error raised when .f not a closure", {
  errmsg <- "`.f` not an interpreted function"
  bad_fns <- list(NULL, NA, log, 1, "A", quote(ls))

  for (f in bad_fns) {
    expect_error(loosely(f), errmsg)
  }
})

test_that("error raised when .quiet not TRUE/FALSE", {
  f <- function(x) NULL

  # No error if .quiet TRUE/FALSE or not supplied
  expect_error(loosely(f, .quiet = TRUE), NA)
  expect_error(suppressWarnings(loosely(f, .quiet = FALSE)), NA)
  expect_error(suppressWarnings(loosely(f)), NA)

  # Otherwise, error
  errmsg <- "`.quiet` not TRUE/FALSE"
  bad_val <- list(NULL, NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(loosely(f, .quiet = val), errmsg)
  }
})

test_that("warning when not firm closure, unless .quiet = TRUE", {
  for (f in fs) {
    expect_warning(loosely(f, .quiet = FALSE),
                   "Argument not a firmly applied function")
    expect_warning(loosely(f),
                   "Argument not a firmly applied function")
    expect_warning(loosely(f, .quiet = TRUE), NA)
  }
})

test_that("original function is restored", {
  for (f in fs) {
    f_firm <- firmly(f, ~ is.numeric)
    expect_identical(loosely(f_firm, .quiet = TRUE), f)

    if (any(nomen(formals(f))$wo_value)) {
      f_warn     <- firmly(f, .warn_missing = TRUE)
      f_firmer <- firmly(f_firm, .warn_missing = TRUE)

      expect_identical(loosely(f_warn, .quiet = TRUE), f)
      expect_identical(loosely(f_firmer, .quiet = TRUE), f)
    }
  }
})

test_that("original function environment is restored", {
  for (f in fs) {
    f_firm <- firmly(f, ~ is.numeric)
    expect_identical(environment(loosely(f_firm, .quiet = TRUE)), environment(f))
  }
})

test_that("original function attributes are restored", {
  set.seed(1)

  for (f in fs) {
    attr <- setNames(as.list(sample(LETTERS)), letters)
    f <- do.call("structure", c(.Data = f, attr))
    f_firm <- firmly(f, ~ is.numeric)

    expect_identical(attributes(loosely(f_firm, .quiet = TRUE)), attributes(f))
  }
})
