context("Nonstrictly")

fs <- lapply(args_list, pass_args)

test_that("nonstrictly() raises warning if function not a strict closure", {
  for (f in fs) {
    expect_warning(nonstrictly(f, .quiet = FALSE),
                   "Argument not a strictly applied function")
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