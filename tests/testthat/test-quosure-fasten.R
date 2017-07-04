context("Fasten input validation checks")

test_that("error raised when error_class is not NULL or a character vector", {
  f <- function(x) NULL
  expect_error(fasten(error_class = NULL), NA)
  expect_error(fasten(error_class = character()), NA)
  expect_error(fasten(error_class = letters), NA)
  non_chr <- list(NA, 0, log, mtcars)
  for (x in non_chr)
    expect_error(
      fasten(error_class = x),
      "'error_class' must be NULL or a character vector without NAs"
    )
})

test_that("fasten() is a curried version of firmly()", {
  f <- function(x, y, ...) NULL
  f1 <- firmly(f, "{{.}} isn't true" := isTRUE ~ vld(x, "y is not true" := y))
  f2 <- fasten("{{.}} isn't true" := isTRUE ~ vld(x, "y is not true" := y))(f)

  expect_identical(NULL, expect_identical(f1(TRUE, TRUE), f2(TRUE, TRUE)))
  expect_error(f1(TRUE, 0), "y is not true")
  expect_error(f2(TRUE, 0), "y is not true")
  expect_error(f1(0, TRUE), "x isn't true")
  expect_error(f2(0, TRUE), "x isn't true")
})
