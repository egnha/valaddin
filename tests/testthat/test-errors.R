context("Error messages")

test_that("unnamed global check uses auto-generated error messages", {
  f <- firmly(function(x, y) NULL, isTRUE)
  expect_error(f(x = FALSE), errmsg_false("isTRUE(x)"))
  expect_error(f(y = FALSE), errmsg_false("isTRUE(y)"))
})

test_that("unnamed local check uses auto-generated error messages", {
  f <- firmly(function(x, y) NULL, isTRUE ~ vld(x, y))
  expect_error(f(FALSE, TRUE), errmsg_false("isTRUE(x)"))
  expect_error(f(TRUE, FALSE), errmsg_false("isTRUE(y)"))
})

test_that("named global check uses name as error message", {
  f <- firmly(function(x, y) NULL, "Not true" := isTRUE)
  expect_n_errors(1, f, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(2, f, list(x = FALSE, y = FALSE), "Not true")
})

test_that("named local check uses name as error message", {
  f <- firmly(function(x, y) NULL, "Not true" := isTRUE ~ vld(x, y))
  expect_n_errors(1, f, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(1, f, list(x = TRUE, y = FALSE), "Not true")
  expect_n_errors(2, f, list(x = FALSE, y = FALSE), "Not true")
})

test_that("name of local expression overrides name of check", {
  f <- firmly(function(x, y) NULL, "global" := isTRUE ~ vld("local" := x, y))
  expect_error(f(FALSE, TRUE), "local")
  expect_error(f(TRUE, FALSE), "global")
})

test_that("name of local expression overrides auto-generated error message", {
  f <- firmly(function(x, y) NULL, isTRUE ~ vld("local" := x, y))
  expect_error(f(FALSE, TRUE), "local")
  expect_error(f(TRUE, FALSE), errmsg_false("isTRUE(y)"))
})

test_that("auto-message is used if error message fails to be created", {
  f <- firmly(function(x, y) NULL, "{stop('!')}" := isTRUE ~ x)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))
  expect_error(f(FALSE), "Error interpolating message")
})

# String-interpolation of error messages ----------------------------------

context("String-interpolation of error messages")

test_that("error messages of named global check interpolate dot", {
  f <- local({
    s_quote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      function(x, y) NULL,
      "{{s_quote(.)}} is not a scalar (length is {length(.)})" := is_scalar
    )
  })
  expect_error(f(x = 1:3), esc_perl("'x' is not a scalar (length is 3)"))
  expect_error(f(y = 1:2), esc_perl("'y' is not a scalar (length is 2)"))
})

test_that("dot in global check message always stands for current argument", {
  f <- firmly(function(x, .) NULL, "literal: {{.}}; value: {.}" := isTRUE)
  expect_error(f(TRUE, "dot-value"), "literal: .; value: dot-value")
  expect_error(f("x-value", TRUE), "literal: x; value: x-value")
})

test_that("dot in local check message does not stand for current argument", {
  f <- firmly(function(x, .) NULL,
              "literal: {{.}}, value: {.}" := isTRUE ~
                vld("x: {x}, dot: {.}" := x, .))
  expect_error(f(TRUE, "dot"), "literal: ., value: dot")
  expect_error(f("x", TRUE), "x: x, dot: TRUE")
})

test_that("error messages of named local check interpolate dot", {
  f <- local({
    s_quote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      function(x, y) NULL,
      "{{s_quote(.)}} is not a scalar (length is {length(.)})" :=
        is_scalar ~ vld(x, x - y)
    )
  })
  expect_error(f(1:2),
               esc_perl("'x' is not a scalar (length is 2)"))
  expect_error(f(1:2, 1:2),
               esc_perl("'x' is not a scalar (length is 2)"))
  expect_error(f(1:3, 4:6),
               esc_perl("'x - y' is not a scalar (length is 3)"))
})

test_that("error messages of unnamed global check don't interpolate dot", {
  f <- firmly(function(x, y) NULL, {.})
  expect_error(f(FALSE, TRUE), errmsg_false("(function(.) {.})(x)"))
  expect_error(f(TRUE, FALSE), errmsg_false("(function(.) {.})(y)"))
})

test_that("error messages of unnamed local check don't interpolate dot", {
  f <- firmly(function(x, y) NULL, {.} ~ x)
  expect_error(f(FALSE, TRUE), errmsg_false("(function(.) {.})(x)"))
  expect_error(f(TRUE, FALSE), NA)
})

test_that("message of global check is interpolated in the messages's scope", {
  chk <- local({
    bar <- function(x) sprintf("local text: %s", x)
    vld("{{bar(.)}}; value: {.}" := isTRUE)
  })
  bar <- function(x) "bogus"  # should be ignored by string interpolation
  f <- firmly(function(x) NULL, UQS(chk))
  expect_error(f(TRUE), NA)
  expect_error(f("not true"), "local text: x; value: not true")
})

test_that("message of check item is interpolated in the messages's scope", {
  chk <- local({
    message <- "local message"
    isTRUE ~ vld("{message}" := x)
  })
  message <- "bogus"  # should be ignored by string interpolation
  f <- firmly(function(x, y) NULL, chk)
  expect_error(f(FALSE, "y"), "local message")
})
