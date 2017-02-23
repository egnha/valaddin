context("Printing")

# Like expect_output(), but gracefully accepts expectation strings w/o escaping
expect_output_p <- perlize(expect_output)

has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
fs <- lapply(args_list[has_xy], pass_args)
chks <- list(
  list("Not numeric" ~ x) ~ is.numeric,
  list("Not character" ~ y) ~ is.character
)
fs_strict <- lapply(fs, strictly, .checklist = chks)

test_that("original function body is displayed", {
  for (i in seq_along(fs)) {
    original_fn <- capture_output(print(fs[[i]]))
    expect_output_p(print(fs_strict[[i]]), original_fn)
  }
})

test_that("checks are displayed", {
  for (f in fs_strict) {
    expect_output_p(print(f), "is.numeric(x):\n\"Not numeric\"")
    expect_output_p(print(f), "is.character(y):\n\"Not character\"")
  }
})

test_that("arguments whose absence is checked are displayed", {
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

test_that("local checker predicate is displayed", {})

test_that("local checker error message is displayed", {})