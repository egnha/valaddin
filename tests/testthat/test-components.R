context("Components")

fs <- lapply(args_list, pass_args)

test_that("firm_core gets original function of a firmly applied function", {
  for (f in fs) {
    nm <- nomen(formals(f))$nm
    if (!length(nm)) {
      # No named argument, so firmly does not create firm closure
      expect_null(firm_core(suppressWarnings(firmly(f, ~ is.numeric))))

      next
    }

    f_firm <- firmly(f, ~ is.numeric)
    f_warn <- firmly(f, .warn_missing = nm)
    f_firm_warn <- firmly(f_firm, .warn_missing = nm)

    expect_identical(firm_core(f_firm), f)
    expect_identical(firm_core(f_warn), f)
    expect_identical(firm_core(f_firm_warn), f)
  }
})

test_that("firm_core returns NULL for non-firmly applied functions", {
  for (f in fs) {
    expect_null(firm_core(f))
  }
})

test_that("firm_checks gets checks for firmly applied function", {
  fs <- list(function(x, y) NULL, function(x, y, ...) NULL,
             function(...) NULL, function() NULL)

  for (f in fs) {
    nm <- nomen(formals(f))$nm
    if (!length(nm)) {
      # No named argument, so firmly does not create firm closure
      expect_null(suppressWarnings(firm_checks(firmly(f, ~ is.numeric))))

      next
    }

    is_positive <- function(x) x > 0
    is_numeric <- purrr::is_scalar_numeric
    chks <- list(
      ~ is_numeric,
      list(~ x, "y not greater than x" ~ y - x) ~ is_positive
    )
    ffs <- list(
      firmly(f, .warn_missing = "y"),
      firmly(f, .checklist = chks),
      firmly(f, .checklist = chks, .warn_missing = "x")
    )
    exprs <- list(
      "FALSE: is_numeric(x)"  = substitute(f(x),     list(f = is_numeric)),
      "FALSE: is_numeric(y)"  = substitute(f(y),     list(f = is_numeric)),
      "FALSE: is_positive(x)" = substitute(f(x),     list(f = is_positive)),
      "y not greater than x"  = substitute(f(y - x), list(f = is_positive))
    )

    # No checks if we only check for missing arguments
    expect_null(firm_checks(ffs[1L]))

    # Checks in chks are correctly encoded in chks_df
    for (ff in ffs[-1L]) {
      chks_df <- firm_checks(ff)

      # Exactly four checks
      expect_equal(nrow(chks_df), 4L)

      for (msg in names(exprs)) {
        expect_identical(chks_df[chks_df$msg == msg, ]$expr[[1L]], exprs[[msg]])
      }
    }
  }
})

test_that("firm_checks returns NULL for non-firmly applied functions", {
  for (f in fs) {
    expect_null(firm_checks(f))
  }
})

test_that("firm_args gets missing-arguments for firmly applied function", {
  for (f in fs) {
    nm <- nomen(formals(f))$nm
    if (!length(nm)) {
      # Should raise error when no named argument but .warn_missing given
      expect_error(firm_args(firmly(f, .warn_missing = "x")),
                   "Invalid `\\.warn_missing`: `\\.f` has no named argument")

      next
    }

    f_firm <- firmly(f, ~ is.numeric)

    # No missing-argument warnings applied
    expect_null(firm_args(f_firm))

    # Missing-argument warning (with and without checks)
    nm_seq <- lapply(seq_along(nm), function(.) nm[seq_len(.)])
    for (n in nm_seq) {
      f_warn <- firmly(f, .warn_missing = n)
      f_firm_warn <- firmly(f_firm, .warn_missing = n)

      expect_equal(firm_args(f_warn), n)
      expect_equal(firm_args(f_firm_warn), n)
    }
  }
})

test_that("firm_args returns NULL for non-firmly applied functions", {
  for (f in fs) {
    expect_null(firm_args(f))
  }
})
