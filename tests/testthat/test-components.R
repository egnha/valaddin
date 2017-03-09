context("Components")

fs <- lapply(args_list, pass_args)
has_args <- purrr::map_lgl(args_list, ~ length(nomen(.)$nm) > 0L)
fs_with_args <- fs[has_args]

test_that("firm_core() gets original function", {
  for (f in fs_with_args) {
    f_firm <- firmly(f, ~is.numeric)
    expect_identical(firm_core(f_firm), f)
  }
})

test_that("firm_checks() gets checks", {
  f <- function(x, y, ...) NULL
  is_positive <- function(x) x > 0
  is_numeric <- purrr::is_scalar_numeric
  chks <- list(
    ~ is_numeric,
    list(~ x, "y not greater than x" ~ y - x) ~ is_positive
  )
  f_firm <- firmly(f, .checklist = chks)
  chks_df <- firm_checks(f_firm)

  # Exactly four checks
  expect_equal(nrow(chks_df), 4L)

  exprs <- list(
    "FALSE: is_numeric(x)" = substitute(f(x), list(f = is_numeric)),
    "FALSE: is_numeric(y)" = substitute(f(y), list(f = is_numeric)),
    "FALSE: is_positive(x)" = substitute(f(x), list(f = is_positive)),
    "y not greater than x" = substitute(f(y - x), list(f = is_positive))
  )

  # Checks in chks are correctly encoded in chks_df
  for (msg in names(exprs)) {
    expect_identical(chks_df[chks_df$msg == msg, ]$expr[[1L]], exprs[[msg]])
  }
})

test_that("firm_args() gets names of arguments whose absence is checked", {
  has_args_wo_value <- purrr::map_lgl(args_list, ~ any(nomen(.)$wo_value))

  for (f in fs[has_args_wo_value]) {
    f_firm <- firmly(f, .warn_missing = TRUE)
    arg <- nomen(formals(f_firm))
    nms_arg_wo_value <- sort(arg$nm[arg$wo_value])

    expect_identical(sort(firm_args(f_firm)), nms_arg_wo_value)
  }
})