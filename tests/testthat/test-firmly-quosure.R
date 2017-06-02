context("Quosure-based firmly")

foo <- function(x, ..., y = 0) invisible(NULL)

errmsg_false <- function(text) {
  message_false(esc_perl(text))
}
errmsg_error <- function(text) {
  sprintf("Error evaluating check %s", esc_perl(text))
}
errmsg_invalid <- function(expr, val) {
  expr <- esc_perl(expr)
  val  <- esc_perl(val)
  sprintf("Predicate value %s not TRUE/FALSE: %s", expr, val)
}

# Global-scope check ------------------------------------------------------
context("Global-scope check")

test_that("global check raises no error when all checks pass", {
  f <- firmly(foo, is.numeric)
  expect_error(f(1), NA)
  expect_error(f(1:3), NA)
})

test_that("global check raises error for each and every failing check", {
  f <- firmly(foo, is.numeric)
  expect_n_errors(1, f, list(x = "x"), errmsg_false("is.numeric(x)"))
  expect_n_errors(1, f, list(y = "y"), errmsg_false("is.numeric(y)"))
  expect_n_errors(2, f, list(x = "x", y = "y"), errmsg_false("is.numeric"))
})

test_that("global check raises error for checks that fail to evaluate", {
  f <- firmly(foo, is.numeric)
  expect_error(f(), errmsg_error("is.numeric(x)"))
  expect_error(f(y = "y"), errmsg_error("is.numeric(x)"))
  expect_error(f(stop("!")), errmsg_error("is.numeric(x)"))
})

test_that("global check raises error for checks that return non-TRUE/FALSE", {
  f <- firmly(foo, identity)
  expect_error(f(1), errmsg_invalid("identity(x)", "1"))
  expect_error(f(NA), errmsg_invalid("identity(x)", "NA"))
  expect_error(f(NULL), errmsg_invalid("identity(x)", "NULL"))
})

# Local-scope check -------------------------------------------------------
context("Local-scope check")

test_that("local check of a single expression checks the expression", {
  # check of an argument
  f <- firmly(foo, is.numeric ~ x)
  expect_error(f(1), NA)
  expect_error(f(1, y = stop("!")), NA)
  expect_error(f("1"), "FALSE: is.numeric\\(x\\)")

  # check of an expression of arguments
  is_positive <- function(x) isTRUE(x > 0)
  f <- firmly(foo, is_positive ~ x - y)
  expect_error(f(2), NA)
  expect_error(f(1, y = 0), NA)
  expect_error(f(1, y = 1), "FALSE: is_positive\\(x - y\\)")
})

test_that("local check of quosures checks the quosure expressions", {
  is_positive <- function(x) isTRUE(x > 0)
  f <- firmly(foo, is_positive ~ quos(x, y, x - y))
  expect_error(f(2, y = 1), NA)
  expect_error(f(0, y = 1), "FALSE: is_positive\\(x\\)")
  expect_error(f(0, y = 1), "FALSE: is_positive\\(x - y\\)")
})

# Lambda predicate function -----------------------------------------------
context("Lambda predicate function")

test_that("global lambda expression is interpreted as a predicate function", {
  f <- firmly(foo, {. > 0})
  expect_error(f(1, y = 1), NA)
  expect_error(f(0), "FALSE")
})

test_that("local lambda expression is interpreted as predicate function", {
  f <- firmly(foo, {. > 0} ~ x)
  expect_error(f(1, y = 1), NA)
  expect_error(f(0), "FALSE: \\(function\\(.\\) \\{. > 0\\}\\)\\(x\\)")
})

# Unquoting checks --------------------------------------------------------
context("Unquoting checks")

# Tidy evaluation of checks -----------------------------------------------
context("Tidy evaluation of checks")

test_that("global predicate function is evaluated in environment of check", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private)
  })
  # this should be ignored
  private <- "not private"
  expect_error(f("private"), NA)
  expect_error(f(0), "FALSE: is_private\\(x\\)")
})

test_that("local predicate function is evaluated in environment of check", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private ~ x)
  })
  # this should be ignored
  private <- "not private"
  expect_error(f("private"), NA)
  expect_error(f(0), "FALSE: is_private\\(x\\)")
})

test_that("input validation check is evaluated in environment of check", {
  f <- local({
    private <- "private"
    firmly(function(x) NULL, isTRUE ~ identical(x, private))
  })
  # this should be ignored
  private <- "not private"
  expect_error(f("private"), NA)
  expect_error(f("not private"), "FALSE: isTRUE\\(identical\\(x, private\\)\\)")
})

test_that("check expression doesn't clash with names in calling frame", {
  f <- local({
    private <- "private"
    firmly(function(x) NULL, isTRUE ~ identical(x, private))
  })
  # this should be ignored
  private <- "not private"
  expect_error(f("private"), NA)
  expect_error(f(private), "FALSE: isTRUE\\(identical\\(x, private\\)\\)")
})

test_that("predicate function doesn't clash with names in calling frame", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private ~ x)
  })
  is_private <- function(...) FALSE
  expect_error(f("private"), NA)
  expect_error(f("not private"), "FALSE: is_private\\(x\\)")
})
