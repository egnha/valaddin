# Like expect_output(), but accepts expectation strings with less escaping
expect_output_p <- perlize(expect_output)

# firm_closure ----------------------------------------------------------

context("Printing firm closures")

has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
fs <- lapply(args_list[has_xy], pass_args)
chks <- list(
  list("Not numeric" ~ x) ~ is.numeric,
  list("Not character" ~ y) ~ is.character
)
fs_firm <- lapply(fs, firmly, .checklist = chks)

test_that("firm closure original function body is displayed", {
  for (i in seq_along(fs)) {
    original_fn <- capture_output(print.default(fs[[i]]))
    expect_output_p(print(fs_firm[[i]]), original_fn)
  }
})

test_that("firm closure checks are displayed", {
  for (f in fs_firm) {
    expect_output_p(print(f), "is.numeric(x):\n\"Not numeric\"")
    expect_output_p(print(f), "is.character(y):\n\"Not character\"")
  }
})

test_that("firm closure error subclass is displayed", {
  for (f in fs_firm) {
    g <- firmly(f, .error_class = c("extraSpecialError", "specialError"))

    expect_output_p(print(f), "check errors:\nsimpleError")
    expect_output_p(print(g), "check errors:\nextraSpecialError, specialError")
  }

  expect_output_p(
    print(firmly(function(x) x, .warn_missing = "x")),
    "check errors:\nNone"
  )
})

test_that("firm closure arguments whose absence is checked are displayed", {
  nms <- list("x", "y", c("x", "y"), c("y", "x"))

  for (f in fs) {
    f_firm <- firmly(f, .checklist = chks)
    expect_output_p(print(f_firm), "missing arguments:\nNot checked")

    for (nm in nms) {
      f_warn <- firmly(f, .warn_missing = nm)
      f_firm_warn <- firmly(f_firm, .warn_missing = nm)

      msg <- paste0("missing arguments:\n",
                    paste(encodeString(nm, quote = "`"), collapse = ", "))
      expect_output_p(print(f_warn), msg)
      expect_output_p(print(f_firm_warn), msg)
    }
  }
})

# check_maker -------------------------------------------------------------

context("Printing check makers")

test_that("local checker predicate is displayed", {
  header <- "* Predicate function:"

  fmls <- list(
    "Not data frame" ~ is.data.frame,
    "Not unsorted" ~ is.unsorted,
    "Not NaN" ~ function(x) isTRUE(is.nan(x)),
    "Not positive" ~ function(x) x > 0
  )

  for (f in fmls) {
    pred <- lazyeval::f_rhs(f)
    out <- paste(header, capture_output(print.default(pred)), sep = "\n")

    expect_output_p(print(localize(f)), out)
  }
})

test_that("local checker error message is displayed", {
  header <- "* Error message:"

  # vld_numeric, vld_scalar_numeric, etc., have exceptional error messages
  nms_chkrs <- setdiff(
    grep("^vld_", getNamespaceExports("valaddin"), value = TRUE),
    c("vld_numeric", "vld_scalar_numeric", "vld_true", "vld_false", "vld_any",
      "vld_all", "vld_na", "vld_nan")
  )
  chkrs <- lapply(nms_chkrs, getExportedValue, ns = "valaddin")
  names(chkrs) <- sub("^vld_", "", nms_chkrs)

  for (nm in names(chkrs)) {
    msg <- sprintf("Not %s", gsub("_", " ", nm))
    out <- paste(header, encodeString(msg, quote = "\""), sep = "\n")

    expect_output_p(print(chkrs[[nm]]), out)
  }
})

# Error messages ----------------------------------------------------------

context("Printing error messages")

test_that("call displays all default values", {
  foo <- firmly(function(x, y = 1, z = f(x, y), w, ..., a = 2) NULL,
                list(~x) ~ is.numeric)
  expect_error(foo("1"), 'foo\\(x = "1", y = 1, z = f\\(x, y\\), a = 2\\)')
})

test_that("call does not display arguments without value", {
  foo <- firmly(function(x, y = 1, absent) NULL, list(~x) ~ is.numeric)
  errmsg <- tryCatch(foo("1"), error = conditionMessage)
  expect_false("absent" %in% errmsg)
})

test_that("call displays all arguments with supplied value", {
  foo <- firmly(function(x, y, z) NULL, list(~x) ~ is.numeric)
  expect_error(foo("1", z = 0), 'foo\\(x = "1", z = 0\\)')
})
