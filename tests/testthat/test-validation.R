context("Input validation")

test_that("function is reproduced when all checks pass", {
})

test_that("one-sided formula produces global check", {
  f <- function(x, y, z = 0, ..., u = y - z, v) x + y + z + u
  f_num <- strictly(f, ~ is.numeric)
  f_pos <- strictly(f_num, ~ {. > 0})

  # Pass
  out <- f(1, 2, 3, u = 4, v = 5)
  expect_identical(f_num(1, 2, 3, u = 4, v = 5), out)
  expect_identical(f_pos(1, 2, 3, u = 4, v = 5), out)

  # Check failure
  nms <- c("x", "y", "z", "u", "v")
  arg_list <- lapply(1:3, function(n) {
    as.list((1:5) * c(rep(-1, n), rep(1, 5 - n))) %>% setNames(nms)
  })
  for (i in seq_along(arg_list)) {
    for (arg in nms[1:i]) {
      expect_error(purrr::lift(f_pos)(arg_list[[i]]),
                   sprintf("FALSE[^\n]*?\\(%s\\)", arg))
    }
    # No other errors
    expect_equal(purrr::lift(purrr::safely(f_pos))(arg_list[[i]]) %>% {
      str_count(.$error, "FALSE")
    }, i)
  }

  # Error evaluating check because of missing argument
  expect_equal(f(1, 2), 1 + 2 + 0 + 2 - 0)
  expect_error(f_num(1, 2), "Error evaluating check.*?argument \"v\" is missing")
  expect_error(f_pos(1, 2), "Error evaluating check.*?argument \"v\" is missing")

  # Error evaluating check because of invalid input types
  expect_error(f_pos(1, "y", v = 0), "FALSE[^\n]*?is\\.numeric\\(y\\)")
  expect_error(f_pos(1, "y", v = 0),
               "FALSE[^\n]*?\\(\\~\\{\\. > 0\\}\\)\\)\\(z\\)")
  expect_error(f_pos(1, "y", v = 0),
               "FALSE[^\n]*?\\(\\~\\{\\. > 0\\}\\)\\)\\(v\\)")
  expect_error(f_pos(1, "y", v = 0),
               "Error evaluating check.*?is\\.numeric\\(u\\)")
  expect_error(f_pos(1, "y", v = 0),
               "Error evaluating check.*?\\(\\~\\{\\. > 0\\}\\)\\)\\(u\\)")
  # No other errors
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "FALSE")
  }, 3)
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Error evaluating check")
  }, 2)
})

test_that("string formula produces global check with message", {})

test_that("unnamed checks in checklist formula use auto-generated messages", {})

test_that("named checks in checklist formula use custom messages", {})

test_that("lifted predicate function check argument list", {})

test_that("anonymous predicate function is correctly interpreted", {})