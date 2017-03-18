context("Printing")

# Like expect_output(), but accepts expectation strings with less escaping
expect_output_p <- perlize(expect_output)

# firm_closure ----------------------------------------------------------

has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
fs <- lapply(args_list[has_xy], pass_args)
chks <- list(
  list("Not numeric" ~ x) ~ is.numeric,
  list("Not character" ~ y) ~ is.character
)
fs_firm <- lapply(fs, firmly, .checklist = chks)

test_that("firm closure original function body is displayed", {
  for (i in seq_along(fs)) {
    original_fn <- capture_output(print(fs[[i]]))
    expect_output_p(print(fs_firm[[i]]), original_fn)
  }
})

test_that("firm closure checks are displayed", {
  for (f in fs_firm) {
    expect_output_p(print(f), "is.numeric(x):\n\"Not numeric\"")
    expect_output_p(print(f), "is.character(y):\n\"Not character\"")
  }
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

test_that("local checker predicate is displayed", {
  header <- "* Predicate function:"

  fmls <- list(
    "Not data frame" ~ is.data.frame,
    "Not unsorted" ~ is.unsorted,
    "Not NaN" ~ is.nan,
    "Not positive" ~ function(x) x > 0
  )

  for (f in fmls) {
    pred <- lazyeval::f_rhs(f)
    out <- paste(header, capture_output(pred), sep = "\n")

    expect_output_p(print(localize(f)), out)
  }
})

test_that("local checker error message is displayed", {
  header <- "* Error message:"

  # vld_numeric, vld_scalar_numeric have exceptional error messages
  nms_chkrs <- setdiff(
    grep("^vld_", getNamespaceExports("valaddin"), value = TRUE),
    c("vld_numeric", "vld_scalar_numeric", "vld_true", "vld_false")
  )
  chkrs <- lapply(nms_chkrs, getExportedValue, ns = "valaddin")
  names(chkrs) <- sub("^vld_", "", nms_chkrs)

  for (nm in names(chkrs)) {
    msg <- sprintf("Not %s", gsub("_", " ", nm))
    out <- paste(header, encodeString(msg, quote = "\""), sep = "\n")

    expect_output_p(print(chkrs[[nm]]), out)
  }
})
