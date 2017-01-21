context("Nonstrictly")

fs <- lapply(args_list, pass_args)
has_args <- purrr::map_lgl(args_list, ~ length(nomen(.)$nm) > 0L)
fs_with_args <- fs[has_args]

test_that("nonstrictly() fails if function is not a strict closure", {
  for (f in fs) {
    expect_error(nonstrictly(f), "Argument not a strictly applied function")
  }
})

test_that("nonstrictly() restores original function", {
  for (f in fs_with_args) {
    f_strict <- strictly(f, ~ is.numeric)
    expect_identical(nonstrictly(f_strict), f)

    if (any(nomen(formals(f))$wo_value)) {
      f_warn     <- strictly(f, .warn_missing = TRUE)
      f_stricter <- strictly(f_strict, .warn_missing = TRUE)

      expect_identical(nonstrictly(f_warn), f)
      expect_identical(nonstrictly(f_stricter), f)
    }
  }
})

test_that("nonstrictly() restores original function environment", {
  for (f in fs_with_args) {
    f_strict <- strictly(f, ~ is.numeric)
    expect_identical(environment(nonstrictly(f_strict)), environment(f))
  }
})

test_that("nonstrictly() restores original function attributes", {
  set.seed(1)

  for (f in fs_with_args) {
    attr <- setNames(as.list(sample(LETTERS)), letters)
    f <- do.call("structure", c(.Data = f, attr))
    f_strict <- strictly(f, ~ is.numeric)

    expect_identical(attributes(nonstrictly(f_strict)), attributes(f))
  }
})