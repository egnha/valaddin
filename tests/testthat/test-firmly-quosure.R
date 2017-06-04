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
  expect_error(f(1, y = stop("!")), NA)
  expect_error(f("1"), errmsg_false("is.numeric(x)"))

  # check of an expression of arguments
  is_positive <- function(x) isTRUE(x > 0)
  f <- firmly(foo, is_positive ~ x - y)
  expect_error(f(2), NA)
  expect_error(f(1, y = 0), NA)
  expect_error(f(1, y = 1), errmsg_false("is_positive(x - y)"))
})

test_that("local check of quosures checks the quosure expressions", {
  is_positive <- function(x) isTRUE(x > 0)
  f <- firmly(foo, is_positive ~ quos(x, y, x - y))
  expect_error(f(2, y = 1), NA)
  expect_error(f(0, y = 1), errmsg_false("is_positive(x)"))
  expect_error(f(0, y = 1), errmsg_false("is_positive(x - y)"))
})

# Lambda predicate function -----------------------------------------------
context("Lambda predicate function")

test_that("global lambda expression is interpreted as a predicate function", {
  z <- 0
  f <- firmly(foo, {. > z})
  expect_error(f(1, y = 1), NA)
  expect_error(f(0), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
})

test_that("local lambda expression is interpreted as predicate function", {
  z <- 0
  f <- firmly(foo, {. > z} ~ x)
  expect_error(f(1, y = stop("!")), NA)
  expect_error(f(0), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
})

# Unquoting checks --------------------------------------------------------
context("Unquoting checks")

test_that("global-check predicate can be unquoted", {
  predicate1 <- local({
    z <- 0
    rlang::quo(function(x) x > z)
  })
  predicate2 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  zero <- 0
  f <- firmly(foo, !! predicate1, !! predicate2, {. > !!zero})
  expect_error(f(x = 1, y = 1), NA)
  expect_error(f(x = 0, y = 1), errmsg_false("(function(x) x > z)(x)"),
               perl = TRUE)
  expect_error(f(x = 0, y = 1), errmsg_false("(function(.) {. > z})(x)"),
               perl = TRUE)
  expect_error(f(x = 0, y = 1), errmsg_false("(function(.) {. > 0})(x)"),
               perl = TRUE)
})

test_that("local-check predicate can be unquoted", {
  predicate1 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  predicate2 <- local({
    z <- 0
    rlang::quo((function(x) x > z))
  })
  zero <- 0
  f <- firmly(foo, UQ(predicate1) ~ x, {. > !!zero} ~ x, UQ(predicate2) ~ y)
  expect_error(f(x = 1, y = 1), NA)
  expect_error(f(x = 0, y = 0),
               errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
  expect_error(f(x = 0, y = 0),
               errmsg_false("(function(.) {. > 0})(x)"), perl = TRUE)
  expect_error(f(x = 0, y = 0),
               errmsg_false("(function(x) x > z)(y)"), perl = TRUE)
})

test_that("local check expressions can be unquoted", {
  q <- local({
    one <- 1
    rlang::quo(y - one)
  })
  two <- 2
  f <- firmly(foo, {. > 0} ~ quos(x - !!two, !!q))
  expect_error(f(x = 3, y = 2), NA)
  expect_error(f(x = 2, y = 1),
               errmsg_false("(function(.) {. > 0})(x - 2)"), perl = TRUE)
  expect_error(f(x = 2, y = 1),
               errmsg_false("(function(.) {. > 0})(y - one)"), perl = TRUE)
})

test_that("error message for global check can be unquoted", {
  msg <- "'{{.}}' is not numeric: {.}"
  f <- firmly(foo, !!msg := is.numeric)
  expect_error(f(x = 0, y = 1), NA)
  expect_error(f(x = "text", y = 1), "'x' is not numeric: text")
})

test_that("error message for local check can be unquoted", {
  msg <- "'x' is not numeric: {x}"
  f <- firmly(foo, is.numeric ~ quos(!!msg := x))
  expect_error(f(x = 0, y = 1), NA)
  expect_error(f(x = "text", y = 1), "'x' is not numeric: text")
})

# Tidy evaluation of checks -----------------------------------------------
context("Tidy evaluation of checks")

test_that("global predicate function is evaluated in environment of check", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private)
  })
  private <- "not private"  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f(0), errmsg_false("is_private(x)"))
})

test_that("local predicate function is evaluated in environment of check", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private ~ x)
  })
  private <- "not private"  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("is_private(x)"))
})

test_that("input validation is evaluated in environment of check", {
  f <- local({
    private <- "private"
    predicate <- base::identical
    firmly(function(x) NULL, isTRUE ~ predicate(x, private))
  })
  private <- "not private"         # private and predicate
  predicate <- function(...) TRUE  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("isTRUE(predicate(x, private))"))
})

test_that("check expression doesn't clash with names in calling frame", {
  f <- local({
    private <- "private"
    firmly(function(x) NULL, isTRUE ~ identical(x, private))
  })
  private <- "not private"  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f(private), errmsg_false("isTRUE(identical(x, private))"))
})

test_that("predicate function doesn't clash with names in calling frame", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private ~ x)
  })
  is_private <- function(...) FALSE  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("is_private(x)"))
})

# Error messages ----------------------------------------------------------
context("Error messages")

test_that("unnamed global check uses auto-generated error messages", {
  f <- firmly(foo, is.numeric)
  expect_error(f(x = "x"), errmsg_false("is.numeric(x)"))
  expect_error(f(y = "y"), errmsg_false("is.numeric(y)"))
})

test_that("unnamed local check uses auto-generated error messages", {
  f <- firmly(foo, is.numeric ~ quos(x, y))
  expect_error(f(x = "0", y = 1), errmsg_false("is.numeric(x)"))
  expect_error(f(x = 0, y = "1"), errmsg_false("is.numeric(y)"))
})

test_that("named global check uses name as error message", {
  f <- firmly(foo, "Not numeric" = is.numeric)
  expect_n_errors(1, f, list(x = "0", y = 1), "Not numeric")
  expect_n_errors(2, f, list(x = "0", y = "1"), "Not numeric")
})

test_that("named local check uses name as error message", {
  f <- firmly(foo, "Not numeric" = is.numeric ~ quos(x, y))
  expect_n_errors(1, f, list(x = "0", y = 1), "Not numeric")
  expect_n_errors(1, f, list(x = 0, y = "1"), "Not numeric")
  expect_n_errors(2, f, list(x = "0", y = "1"), "Not numeric")
})

test_that("name of local expression overrides name of check", {
  f <- firmly(foo, "Not numeric" = is.numeric ~ quos("local name" = x, y))
  expect_error(f(x = "0", y = 1), "local name")
  expect_error(f(x = 0, y = "1"), "Not numeric")
})

test_that("name of local expression overrides auto-generated error message", {
  f <- firmly(foo, is.numeric ~ quos("local name" = x, y))
  expect_error(f(x = "0", y = 1), "local name")
  expect_error(f(x = 0, y = "1"), errmsg_false("is.numeric(y)"))
})

test_that("auto-message is used if error message fails to be created", {
  f <- firmly(foo, "{stop('!')}" = is.numeric ~ x)
  expect_error(f(x = "x", y = 1), errmsg_false("is.numeric(x)"))
  expect_error(f(x = "x", y = 1), "Error interpolating message")
})

context("String-interpolation of error messages")

test_that("error messages of named global check are dot-interpolated", {
  f <- local({
    s_quote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      foo,
      "{{s_quote(.)}} is not a scalar (length is {length(.)})" = is_scalar
    )
  })
  expect_error(f(x = 1:3), esc_perl("'x' is not a scalar (length is 3)"))
  expect_error(f(y = 1:2), esc_perl("'y' is not a scalar (length is 2)"))
})

test_that("dot in global check message always stands for current argument", {
  f <- firmly(function(x, .) NULL, "literal: {{.}}; value: {.}" = isTRUE)
  expect_error(f(TRUE, "dot-value"), "literal: .; value: dot-value")
  expect_error(f("x-value", TRUE), "literal: x; value: x-value")
})

test_that("dot in local check message does not stand for current argument", {
  f <- firmly(function(x, .) NULL,
              "literal: {{.}}, value: {.}" = isTRUE ~
                quos("x: {x}, dot: {.}" = x, .))
  expect_error(f(TRUE, "dot"), "literal: ., value: dot")
  expect_error(f("x", TRUE), "x: x, dot: TRUE")
})

test_that("error messages of named local check are dot-interpolated", {
  f <- local({
    s_quote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      foo,
      "{{s_quote(.)}} is not a scalar (length is {length(.)})" = is_scalar ~
        quos(x, x - y)
    )
  })
  expect_error(f(x = 1:3), esc_perl("'x' is not a scalar (length is 3)"))
  expect_error(f(x = 1:2, y = 1:2),
               esc_perl("'x - y' is not a scalar (length is 2)"))
})

test_that("error messages of unnamed global check are not dot-interpolated", {
  f <- firmly(foo, {.})
  expect_error(f(x = FALSE, y = TRUE), errmsg_false("(function(.) {.})(x)"))
  expect_error(f(x = TRUE, y = FALSE), errmsg_false("(function(.) {.})(y)"))
})

test_that("error messages of unnamed local check are not dot-interpolated", {
  f <- firmly(foo, {.} ~ quos(x))
  expect_error(f(x = FALSE, y = TRUE), errmsg_false("(function(.) {.})(x)"))
  expect_error(f(x = TRUE, y = FALSE), NA)
})

test_that("name of local expression is locally interpolated", {
  f <- local({
    a <- "local"
    firmly(foo, is.numeric ~ quos("x is {x}, a is {a}, y is {y}" = x))
  })
  expect_error(f(x = "x", y = 1), "x is x, a is local, y is 1")
})