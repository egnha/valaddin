context("Loosely")

fs <- lapply(args_list, pass_args)

test_that("error raised when .f not a closure", {
  errmsg <- "`.f` not an interpreted function"
  bad_fns <- list(NULL, NA, log, 1, "A", quote(ls))

  for (f in bad_fns) {
    expect_error(loosely(f), errmsg)
  }
})

test_that("error raised when .keep_check not TRUE/FALSE", {
  f <- firmly(function(x) NULL, ~ is.numeric)

  # No error if .keep_check TRUE/FALSE or not supplied
  expect_error(loosely(f, .keep_check = TRUE), NA)
  expect_error(loosely(f, .keep_check = FALSE), NA)
  expect_error(loosely(f), NA)

  # Otherwise, error
  errmsg <- "`.keep_check` not TRUE/FALSE"
  bad_val <- list(NULL, NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(loosely(f, .keep_check = val), errmsg)
  }
})

test_that("error raised when .keep_warning not TRUE/FALSE", {
  f <- firmly(function(x) NULL, ~ is.numeric)

  # No error if .keep_warning TRUE/FALSE or not supplied
  expect_error(loosely(f, .keep_warning = TRUE), NA)
  expect_error(loosely(f, .keep_warning = FALSE), NA)
  expect_error(loosely(f), NA)

  # Otherwise, error
  errmsg <- "`.keep_warning` not TRUE/FALSE"
  bad_val <- list(NULL, NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(loosely(f, .keep_warning = val), errmsg)
  }
})

test_that("error raised when .quiet not TRUE/FALSE", {
  f <- firmly(function(x) NULL, ~ is.numeric)

  # No error if .quiet TRUE/FALSE or not supplied
  expect_error(loosely(f, .quiet = TRUE), NA)
  expect_error(loosely(f, .quiet = FALSE), NA)
  expect_error(loosely(f), NA)

  # Otherwise, error
  errmsg <- "`.quiet` not TRUE/FALSE"
  bad_val <- list(NULL, NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(loosely(f, .quiet = val), errmsg)
  }
})

test_that("warning signaled when not firm closure and .quiet is FALSE", {
  warning <- "`.f` not a firmly applied function"

  for (f in fs) {
    expect_false(is_firm(f))

    expect_warning(loosely(f, .quiet = FALSE), warning)
    expect_warning(loosely(f, .keep_check = TRUE, .quiet = FALSE), warning)
    expect_warning(loosely(f, .keep_warning = TRUE, .quiet = FALSE), warning)
    expect_warning(
      loosely(f, .keep_check = TRUE, .keep_warning = TRUE, .quiet = FALSE),
      warning
    )

    # .quiet = TRUE should turn off warnings
    expect_warning(loosely(f, .quiet = TRUE), NA)
    expect_warning(loosely(f), NA)
  }
})

test_that("original function restored when .keep_check & .keep_warning FALSE", {
  for (f in fs) {
    # Ordinary function, not firmly applied
    expect_false(is_firm(f))
    expect_identical(loosely(f, .quiet = TRUE), f)
    expect_identical(
      loosely(f, .keep_check = FALSE, .keep_warning = FALSE, .quiet = TRUE),
      f
    )

    nm <- nomen(formals(f))$nm
    # Remainder of tests only apply to firmly applied functions
    if (!length(nm)) next

    # Firm closure with check, but no missing-argument warning
    f_firm <- firmly(f, ~ is.numeric)
    expect_equal(loosely(f_firm, .quiet = TRUE), f)
    expect_equal(
      loosely(f_firm,
              .keep_check = FALSE, .keep_warning = FALSE, .quiet = TRUE),
      f
    )

    # Firm closure with missing-argument warning (with and without checks)
    f_warn <- firmly(f, .warn_missing = nm)
    f_firm_warn <- firmly(f_firm, .warn_missing = nm)

    expect_equal(loosely(f_warn, .quiet = TRUE), f)
    expect_equal(
      loosely(f_warn,
              .keep_check = FALSE, .keep_warning = FALSE, .quiet = TRUE),
      f
    )
    expect_equal(loosely(f_firm_warn, .quiet = TRUE), f)
    expect_equal(
      loosely(f_firm_warn,
              .keep_check = FALSE, .keep_warning = FALSE, .quiet = TRUE),
      f
    )
  }
})

test_that("checks kept only if .keep_check is TRUE", {
  for (f in fs) {
    f_firm <- suppressWarnings(firmly(f, ~ is.numeric))

    # If .keep_check is TRUE, checks are kept
    f_firm_loose <- loosely(f_firm, .keep_check = TRUE)
    expect_equal(f_firm_loose, f_firm)

    nm <- nomen(formals(f))$nm
    # Remainder of tests only apply to firmly applied functions
    if (!length(nm)) next

    expect_true(is_firm(f_firm_loose))
    expect_false(is.null(firm_checks(f_firm_loose)))

    # Checks and warnings
    f_firm_warn <- firmly(f_firm, .warn_missing = nm)
    f_firm_warn_loose <- loosely(f_firm_warn, .keep_check = TRUE)
    expect_equal(f_firm_warn_loose, f_firm)

    # No missing-argument warnings
    expect_null(firm_args(f_firm_warn_loose))

    # But checks are retained
    expect_false(is.null(firm_checks(f_firm_warn_loose)))

    # If .keep_check is FALSE, checks are omitted, original function restored
    expect_null(firm_checks(loosely(f_firm, .keep_check = FALSE)))
    expect_null(firm_checks(loosely(f_firm)))
    expect_equal(loosely(f_firm, .keep_check = FALSE), f)
    expect_equal(loosely(f_firm), f)

    expect_null(firm_checks(loosely(f_firm_warn, .keep_check = FALSE)))
    expect_null(firm_checks(loosely(f_firm_warn)))
    expect_equal(loosely(f_firm_warn, .keep_check = FALSE), f)
    expect_equal(loosely(f_firm_warn), f)
  }
})

test_that("missing-argument warnings kept only if .keep_warning is TRUE", {
  for (f in fs) {
    # If .keep_warning is TRUE but no missing-argument warning, then no change
    f_firm <- suppressWarnings(firmly(f, ~ is.numeric))
    f_firm_loose <- loosely(f_firm, .keep_warning = TRUE, .quiet = TRUE)
    expect_null(firm_args(f_firm))
    expect_null(firm_args(f_firm_loose))

    nm <- nomen(formals(f))$nm
    # Remainder of tests only apply to firmly applied functions
    if (!length(nm)) next

    ffs <- list(
      firmly(f, .warn_missing = nm),
      firmly(f, ~ is.numeric, .warn_missing = nm)
    )

    for (ff in ffs) {
      # If .keep_warning is TRUE, missing-argument warnings are kept
      expect_equal(
        loosely(ff, .keep_warning = TRUE),
        firmly(f, .warn_missing = nm)
      )
      expect_equal(firm_args(loosely(ff, .keep_warning = TRUE)), nm)
      expect_equal(
        firm_args(loosely(ff, .keep_check = TRUE, .keep_warning = TRUE)),
        nm
      )

      # If .keep_warning is FALSE, missing-argument warnings are omitted
      expect_equal(firm_core(ff), f)
      expect_equal(loosely(ff, .keep_warning = FALSE), f)
      expect_equal(loosely(ff), f)
      if (!is.null(firm_checks(ff))) {
        expect_equal(
          loosely(ff, .keep_check = TRUE, .keep_warning = FALSE, .quiet = TRUE),
          f_firm
        )
      }
      expect_null(firm_args(loosely(ff)))
      expect_null(firm_args(loosely(ff, .keep_warning = FALSE)))
      expect_null(firm_args(
        loosely(ff, .keep_check = TRUE, .keep_warning = FALSE)
      ))
    }
  }
})

test_that(".f is returned when not a firmly applied function", {
  for (f in fs) {
    expect_false(is_firm(f))
    expect_identical(
      loosely(f, .keep_check = FALSE, .keep_warning = FALSE, .quiet = TRUE),
      f
    )
  }
})

test_that(".f is returned when both .keep_check & .keep_warning TRUE", {
  for (f in fs) {
    # Ordinary functions (not firm closure)
    expect_identical(loosely(f, .keep_check = TRUE, .keep_warning = TRUE), f)

    nm <- nomen(formals(f))$nm
    if (!length(nm)) next

    # Firmly applied functions (check only, warning only, check and warning)
    f_firm <- firmly(f, ~ is.numeric)
    f_warn <- firmly(f, .warn_missing = nm)
    f_firm_warn <- firmly(f_firm, .warn_missing = nm)
    expect_identical(
      loosely(f_firm, .keep_check = TRUE, .keep_warning = TRUE),
      f_firm
    )
    expect_identical(
      loosely(f_warn, .keep_check = TRUE, .keep_warning = TRUE),
      f_warn
    )
    expect_identical(
      loosely(f_firm_warn, .keep_check = TRUE, .keep_warning = TRUE),
      f_firm_warn
    )
  }
})

test_that("original function environment is restored", {
  for (f in fs) {
    f_firm <- suppressWarnings(firmly(f, ~ is.numeric))
    expect_identical(environment(loosely(f_firm, .quiet = TRUE)), environment(f))
  }
})

test_that("original function attributes are restored", {
  set.seed(1)

  for (f in fs) {
    attr <- setNames(as.list(sample(LETTERS)), letters)
    f <- do.call("structure", c(.Data = f, attr))
    f_firm <- suppressWarnings(firmly(f, ~ is.numeric))

    expect_identical(attributes(loosely(f_firm, .quiet = TRUE)), attributes(f))
  }
})
