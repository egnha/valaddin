context("Attributes of strict closures")

fs <- lapply(args_list, pass_args)
has_args <- purrr::map_lgl(args_list, ~ length(nomen(.)$nm) > 0L)
fs_with_args <- fs[has_args]

test_that("strict_core() extracts original function", {
  for (f in fs_with_args) {
    f_strict <- strictly(f, ~is.numeric)
    expect_identical(strict_core(f_strict), f)
  }
})

test_that("strict_checks() extracts checks", {})

test_that("strict_core() extracts ", {})