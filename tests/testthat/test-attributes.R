context("Attributes of strict closures")

fs <- lapply(args_list, pass_args)
has_args <- purrr::map_lgl(args_list, ~ length(nomen(.)$nm) > 0L)
fs_with_args <- fs[has_args]

test_that("strict_core() gets original function", {
  for (f in fs_with_args) {
    f_strict <- strictly(f, ~is.numeric)
    expect_identical(strict_core(f_strict), f)
  }
})

test_that("strict_checks() gets checks", {
})

test_that("strict_args() gets names of arguments whose absence is checked", {
  has_args_wo_value <- purrr::map_lgl(args_list, ~ any(nomen(.)$wo_value))

  for (f in fs[has_args_wo_value]) {
    f_strict <- strictly(f, .warn_missing = TRUE)
    arg <- nomen(formals(f_strict))
    nms_arg_wo_value <- sort(arg$nm[arg$wo_value])

    expect_identical(sort(strict_args(f_strict)), nms_arg_wo_value)
  }
})