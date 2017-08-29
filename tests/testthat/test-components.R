context("Components")

test_that("firm_core gets original closure of a firmly applied function ", {
  f <- function(x) NULL
  expect_identical(firm_core(firmly(f, is.numeric)), f)
  expect_identical(firm_core(firmly(log, is.numeric)), rlang::as_closure(log))
})

test_that("firm_core returns NULL for non-firmly applied functions", {
  expect_null(firm_core(firmly(log)))
  expect_null(firm_core(unclass(firmly(log, is.numeric))))
})

test_that("firm_checks gets data frame of checks for firmly applied function", {
  f <- firmly(function(x, y) NULL,
              "{{.}} not numeric" := is.numeric,
              {isTRUE(. > 0)}("x not greater than y" := x - y))
  chks <- firm_checks(f)
  nms <- c("pred", "expr", "call", "msg", "env_msg", "is_msg_gbl")
  expect_true(is.data.frame(chks))
  expect_true(setequal(names(chks), nms))
  expect_identical(nrow(chks), 3L)
})

test_that("firm_checks returns NULL for non-firmly applied functions", {
  expect_null(firm_checks(firmly(log)))
  expect_null(firm_checks(unclass(firmly(log, is.numeric))))
})

test_that("firm_error gets error subclass for firmly applied functions", {
  expect_identical(
    firm_error(firmly(log, is.numeric)),
    "inputValidationError"
  )
  expect_identical(
    firm_error(firmly(log, is.numeric, error_class = NULL)),
    "inputValidationError"
  )
  expect_identical(
    firm_error(firmly(log, is.numeric, error_class = "myError")),
    "myError"
  )
  expect_identical(
    firm_error(firmly(log, is.numeric, error_class = c("myError", "myClass"))),
    c("myError", "myClass")
  )
})

test_that("firm_error returns NULL for non-firmly applied functions", {
  expect_null(firm_error(firmly(log)))
  expect_null(firm_error(unclass(firmly(log, is.numeric))))
})
