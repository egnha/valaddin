context("Error messages")

# Using names as error messages -------------------------------------------

context("Using names as error messages")

test_that("unnamed global check uses auto-generated error messages", {
  f <- firmly(function(x, y) NULL, isTRUE)
  expect_error(f(x = FALSE), errmsg_false("isTRUE(x)"))
  expect_error(f(y = FALSE), errmsg_false("isTRUE(y)"))
})

test_that("unnamed local check uses auto-generated error messages", {
  f <- firmly(function(x, y) NULL, isTRUE(x, y))
  expect_error(f(FALSE, TRUE), errmsg_false("isTRUE(x)"))
  expect_error(f(TRUE, FALSE), errmsg_false("isTRUE(y)"))
})

test_that("named global check uses name as error message", {
  f <- firmly(function(x, y) NULL, "Not true" := isTRUE)
  expect_n_errors(1, f, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(2, f, list(x = FALSE, y = FALSE), "Not true")

  is_true <- isTRUE
  error_msg(is_true) <- "Don't use this error message"
  g <- firmly(function(x, y) NULL, "Not true" := is_true)
  expect_n_errors(1, g, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(2, g, list(x = FALSE, y = FALSE), "Not true")
})

test_that("named local check uses name as error message", {
  f <- firmly(function(x, y) NULL, "Not true" := isTRUE(x, y))
  expect_n_errors(1, f, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(1, f, list(x = TRUE, y = FALSE), "Not true")
  expect_n_errors(2, f, list(x = FALSE, y = FALSE), "Not true")
})

test_that("name of local expression overrides name of check", {
  f <- firmly(function(x, y) NULL, "global" := isTRUE("local" := x, y))
  expect_error(f(FALSE, TRUE), "local")
  expect_error(f(TRUE, FALSE), "global")
})

test_that("name of local expression overrides auto-generated error message", {
  f <- firmly(function(x, y) NULL, isTRUE("local" := x, y))
  expect_error(f(FALSE, TRUE), "local")
  expect_error(f(TRUE, FALSE), errmsg_false("isTRUE(y)"))
})

test_that("auto-message is used if error message fails to be created", {
  f <- firmly(function(x, y) NULL, "{stop('!')}" := isTRUE(x))
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))
  expect_error(f(FALSE), "Error interpolating message")
})

# Quasi-predicates --------------------------------------------------------

context("Quasi-predicates")

test_that("quasi-predicate returning string reports string as error message", {
  is_true <- function(x) if (isTRUE(x)) TRUE else "Not true"
  f <-  firmly(function(x) NULL, is_true)
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), "Not true")
})

# String interpolation ----------------------------------------------------

context("String interpolation")

test_that("error messages of named global check interpolate dot", {
  f <- local({
    squote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      function(x, y) NULL,
      "{{squote(.)}} is not a scalar (length is {length(.)})" := is_scalar
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

test_that("dot in local check message matches literal '.' binding", {
  f <- firmly(
    function(x, .) NULL,
    "literal: {{.}}, value: {.}" := isTRUE("x: {x}, dot: {.}" := x, .)
  )
  expect_error(f(TRUE, "dot"), "literal: ., value: dot")
  expect_error(f("x", TRUE), "x: x, dot: TRUE")
})

test_that("error messages of named local check interpolate dot", {
  f <- local({
    squote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      function(x, y) NULL,
      "{{squote(.)}} is not a scalar (length is {length(.)})" :=
        is_scalar(x, x - y)
    )
  })
  expect_error(f(1:2), esc_perl("'x' is not a scalar (length is 2)"))
  expect_error(f(1:2, 1:2), esc_perl("'x' is not a scalar (length is 2)"))
  expect_error(f(1:3, 4:6), esc_perl("'x - y' is not a scalar (length is 3)"))
})

test_that("error messages of unnamed global check don't interpolate dot", {
  f <- firmly(function(x, y) NULL, {.})
  expect_error(f(FALSE, TRUE), errmsg_false("(function (.) {.})(x)"))
  expect_error(f(TRUE, FALSE), errmsg_false("(function (.) {.})(y)"))
})

test_that("error messages of unnamed local check don't interpolate dot", {
  f <- firmly(function(x, y) NULL, {.}(x))
  expect_error(f(FALSE, TRUE), errmsg_false("(function (.) {.})(x)"))
  expect_error(f(TRUE, FALSE), NA)
})

test_that("message of global check is interpolated in the messages's scope", {
  msg <- local({
    bar <- function(x) paste("local text:", x)
    rlang::new_quosure("{{bar(.)}}; value: {.}")
  })
  bar <- function(x) "bogus"  # should be ignored by string interpolation
  f <- firmly(function(x) NULL, !! msg := isTRUE)
  expect_error(f(TRUE), NA)
  expect_error(f("not true"), "local text: x; value: not true")
})

test_that("message of local expression is interpolated in messages's scope", {
  msg <- local({
    message <- "local message"
    rlang::new_quosure("{message}")
  })
  message <- "bogus"  # should be ignored by string interpolation
  f <- firmly(function(x, y) NULL, isTRUE(!! msg := x))
  expect_error(f(FALSE, "y"), "local message")
})

test_that(".expr pronoun captures predicate parameter expression as text", {
  zero <- 0
  false <- function(x, a, b = default) FALSE
  expect_error(firmly(identity, "{{.expr$a}}" := false(zero))(), "zero")
  expect_error(firmly(identity, "{{.expr$a}}" := false(NULL))(), "NULL")
  expect_error(firmly(identity, "{{.expr$b}}" := false(NA))(), "default")
  expect_error(firmly(identity, "{{.expr$b}}" := false(NA, b = zero))(), "zero")
})

test_that(".value pronoun captures predicate parameter value as text", {
  zero <- 0
  false <- function(x, a, b = zero) FALSE
  expect_error(firmly(identity, "{{.value$a}}" := false(zero))(), "0")
  expect_error(firmly(identity, "{{.value$a}}" := false(NULL))(), "NULL")
  expect_error(firmly(identity, "{{.value$b}}" := false(NA))(), "zero")
  expect_error(firmly(identity, "{{.value$b}}" := false(NA, b = zero))(), "0")
})

test_that("error raised if interpolation can't produce string", {
  expect_error(
    firmly(identity, "{.}" := isTRUE)(1:2),
    'Error interpolating message "\\{\\.\\}": not a string \\(has length 2\\)'
  )
  expect_error(
    firmly(identity, "{X}" := isTRUE)(FALSE),
    "Error interpolating message \"\\{X\\}\": object 'X' not found"
  )
  expect_error(
    firmly(identity, "{{NULL}}" := isTRUE)(FALSE),
    "Failed to interpolate as string: '\\{\\{NULL\\}\\}' \\(length 0\\)"
  )
})

# Get and set -------------------------------------------------------------

context("Get and set")

f <- function(x) NULL

test_that("can set error message", {
  is_true <- isTRUE
  env <- new.env()

  error_msg(is_true) <- "new message"
  expect_error(firmly(f, is_true)(FALSE), "new message")

  env$local <- "newer message"
  error_msg(is_true) <- new_error_msg("{local}", env)
  expect_error(firmly(f, is_true)(FALSE), "newer message")

  env$local <- "newest message"
  error_msg(is_true, env) <- "{local}"
  expect_error(firmly(f, is_true)(FALSE), "newest message")
})

test_that("can get error message", {
  is_true <- isTRUE
  error_msg(is_true) <- "message"
  expect_identical(rlang::eval_tidy(error_msg(is_true)), "message")
  expect_identical(rlang::f_env(error_msg(is_true)), environment())
})

test_that("get empty string error message when none set", {
  expect_identical(rlang::eval_tidy(error_msg(isTRUE)), "")
  expect_identical(rlang::f_env(error_msg(isTRUE)), emptyenv())
})
