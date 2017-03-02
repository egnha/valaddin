context("Printing")

# Like expect_output(), but accepts expectation strings with less escaping
expect_output_p <- perlize(expect_output)

# strict_closure ----------------------------------------------------------

has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
fs <- lapply(args_list[has_xy], pass_args)
chks <- list(
  list("Not numeric" ~ x) ~ is.numeric,
  list("Not character" ~ y) ~ is.character
)
fs_strict <- lapply(fs, strictly, .checklist = chks)

test_that("strict closure original function body is displayed", {
  for (i in seq_along(fs)) {
    original_fn <- capture_output(print(fs[[i]]))
    expect_output_p(print(fs_strict[[i]]), original_fn)
  }
})

test_that("strict closure checks are displayed", {
  for (f in fs_strict) {
    expect_output_p(print(f), "is.numeric(x):\n\"Not numeric\"")
    expect_output_p(print(f), "is.character(y):\n\"Not character\"")
  }
})

test_that("strict closure arguments whose absence is checked are displayed", {
  for (f in fs_strict) {
    arg <- nomen(formals(f))
    missing <- arg$nm[arg$wo_value] %>% paste(collapse = ", ")

    f_warn <- strictly(f, .warn_missing = TRUE)

    if (missing == "") {
      expect_output_p(print(f_warn), "missing arguments:\nNot checked")
    } else {
      msg <- paste0("missing arguments:\n", missing)
      expect_output_p(print(f_warn), msg)
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

  nms_chkrs <- grep("^vld_", getNamespaceExports("valaddin"), value = TRUE)
  chkrs <- lapply(nms_chkrs, getExportedValue, ns = "valaddin")
  names(chkrs) <- sub("^vld_", "", nms_chkrs)

  for (nm in names(chkrs)) {
    msg <- sprintf("Not %s", gsub("_", " ", nm))
    out <- paste(header, encodeString(msg, quote = "\""), sep = "\n")

    expect_output_p(print(chkrs[[nm]]), out)
  }
})
