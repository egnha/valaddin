context("Input validation")

test_that("one-sided formula produces global check", {
  f <- pass_args(alist(x = , y = , z = 0, ... = , u = y - z, v = ))
  f_num <- strictly(f, ~is.numeric)
  f_pos <- strictly(f_num, ~{. > 0})

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
  expect_equivalent(f(1, 2), list(1, 2))
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

test_that("string formula produces global check with message", {
  f <- pass_args(alist(x = , y = , z = 0, ... = , u = y - z, v = ))
  f_num <- strictly(f, "Not numeric" ~ is.numeric)
  f_pos <- strictly(f_num, "Not positive" ~ {. > 0})

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
                   sprintf("Not positive: `%s`", arg))
    }
    # No other errors
    expect_equal(purrr::lift(purrr::safely(f_pos))(arg_list[[i]]) %>% {
      str_count(.$error, "Not positive")
    }, i)
  }

  # Error evaluating check because of missing argument
  expect_equivalent(f(1, 2), list(1, 2))
  expect_error(f_num(1, 2), "Error evaluating check.*?argument \"v\" is missing")
  expect_error(f_pos(1, 2), "Error evaluating check.*?argument \"v\" is missing")

  # Error evaluating check because of invalid input types
  expect_error(f_pos(1, "y", v = 0), "Not numeric: `y`")
  expect_error(f_pos(1, "y", v = 0), "Not positive: `z`")
  expect_error(f_pos(1, "y", v = 0), "Not positive: `v`")
  expect_error(f_pos(1, "y", v = 0),
               "Error evaluating check.*?is\\.numeric\\(u\\)")
  expect_error(f_pos(1, "y", v = 0),
               "Error evaluating check.*?\\(\\~\\{\\. > 0\\}\\)\\)\\(u\\)")
  # No other errors
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Not numeric")
  }, 1)
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Not positive")
  }, 2)
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Error evaluating check")
  }, 2)
})

test_that("function value is reproduced when all checks pass", {
  fs <- lapply(args_list, pass_args)

  chklist <- list(~is.numeric, ~{. > 0}, "Not TRUE when coerced" ~ as.logical)

  for (f in fs) {
    l <- length(formals(f))
    # Arguments as list of positive numbers (if not empty)
    args <- if (l) as.list(1:l) else list()
    out <- do.call(f, args)
    f_strict <- strictly(f, .checklist = chklist)

    expect_identical(do.call(f_strict, args), out)
  }
})

test_that("unnamed checks in checklist formula use auto-generated messages", {})

test_that("named checks in checklist formula use custom messages", {})

test_that("lifted predicate function check argument list", {})

test_that("anonymous predicate function is correctly interpreted", {})