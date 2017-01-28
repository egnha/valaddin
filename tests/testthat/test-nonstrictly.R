context("Nonstrictly")

fs <- lapply(args_list, pass_args)

test_that("nonstrictly() raises error if .f not a closure", {
  errmsg <- "`.f` not an interpreted function"
  bad_fns <- list(NULL, NA, log, 1, "A", quote(ls))

  for (f in bad_fns) {
    expect_error(nonstrictly(f), errmsg)
  }
})

test_that("nonstrictly() raises error if .quiet not TRUE/FALSE", {
  f <- function(x) NULL

  # No error if .quiet TRUE/FALSE or not supplied
  expect_error(nonstrictly(f, .quiet = TRUE), NA)
  expect_error(suppressWarnings(nonstrictly(f, .quiet = FALSE)), NA)
  expect_error(suppressWarnings(nonstrictly(f)), NA)

  # Otherwise, error
  errmsg <- "`.quiet` not TRUE/FALSE"
  bad_val <- list(NULL, NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(nonstrictly(f, .quiet = val), errmsg)
  }
})

test_that("nonstrictly() warns if not strict closure unless .quiet = TRUE", {
  for (f in fs) {
    expect_warning(nonstrictly(f, .quiet = FALSE),
                   "Argument not a strictly applied function")
    expect_warning(nonstrictly_(f, .quiet = FALSE),
                   "Argument not a strictly applied function")
    expect_warning(nonstrictly(f),
                   "Argument not a strictly applied function")
    expect_warning(nonstrictly_(f),
                   "Argument not a strictly applied function")
    expect_warning(nonstrictly(f, .quiet = TRUE), NA)
    expect_warning(nonstrictly_(f, .quiet = TRUE), NA)
  }
})

test_that("nonstrictly() restores original function", {
  for (f in fs) {
    f_strict <- strictly(f, ~ is.numeric)
    expect_identical(nonstrictly(f_strict, .quiet = TRUE), f)
    expect_identical(nonstrictly_(f_strict, .quiet = TRUE), f)

    if (any(nomen(formals(f))$wo_value)) {
      f_warn     <- strictly(f, .warn_missing = TRUE)
      f_stricter <- strictly(f_strict, .warn_missing = TRUE)

      expect_identical(nonstrictly(f_warn, .quiet = TRUE), f)
      expect_identical(nonstrictly(f_stricter, .quiet = TRUE), f)
      expect_identical(nonstrictly_(f_warn, .quiet = TRUE), f)
      expect_identical(nonstrictly_(f_stricter, .quiet = TRUE), f)
    }
  }
})

test_that("nonstrictly() restores original function environment", {
  for (f in fs) {
    f_strict <- strictly(f, ~ is.numeric)
    expect_identical(environment(nonstrictly(f_strict, .quiet = TRUE)),
                     environment(f))
    expect_identical(environment(nonstrictly_(f_strict, .quiet = TRUE)),
                     environment(f))
  }
})

test_that("nonstrictly() restores original function attributes", {
  set.seed(1)

  for (f in fs) {
    attr <- setNames(as.list(sample(LETTERS)), letters)
    f <- do.call("structure", c(.Data = f, attr))
    f_strict <- strictly(f, ~ is.numeric)

    expect_identical(attributes(nonstrictly(f_strict, .quiet = TRUE)),
                     attributes(f))
    expect_identical(attributes(nonstrictly_(f_strict, .quiet = TRUE)),
                     attributes(f))
  }
})