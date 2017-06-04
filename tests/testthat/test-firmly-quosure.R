context("Quosure-based firmly")

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
  f <- firmly(function(x, y) NULL, is.numeric)
  expect_error(f(1, 1:3), NA)
  expect_error(f(1:3, 1), NA)
})

test_that("global check raises error for each and every failing check", {
  f <- firmly(function(x, y) NULL, is.numeric)
  expect_n_errors(1, f, list(x = "x"), errmsg_false("is.numeric(x)"))
  expect_n_errors(1, f, list(y = "y"), errmsg_false("is.numeric(y)"))
  expect_n_errors(2, f, list(x = "x", y = "y"), errmsg_false("is.numeric"))
})

test_that("global check raises error for checks that fail to evaluate", {
  f <- firmly(function(x, y) NULL, isTRUE)
  expect_error(f(stop("!")), errmsg_error("isTRUE(x)"))
  expect_error(f(stop("!")), errmsg_error("isTRUE(y)"))
})

test_that("global check raises error for checks that return non-TRUE/FALSE", {
  f <- firmly(function(x, y) NULL, identity)
  expect_error(f(1), errmsg_invalid("identity(x)", "1"))
  expect_error(f(NA), errmsg_invalid("identity(x)", "NA"))
  expect_error(f(NULL), errmsg_invalid("identity(x)", "NULL"))
})

# Local-scope check -------------------------------------------------------
context("Local-scope check")

test_that("check formula with non-quosure RHS checks whole RHS expression", {
  # check of a bare argument
  f <- firmly(function(x, y) NULL, isTRUE ~ x)
  expect_error(f(TRUE, stop("!")), NA)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))

  # check of an expression of arguments
  f <- firmly(function(x, y) NULL, isTRUE ~ length(x) == 1L)
  expect_error(f(1, stop("!")), NA)
  expect_error(f(1:2), errmsg_false("isTRUE(length(x) == 1L)"))
})

test_that("check formula with quosures RHS checks contained expressions", {
  is_positive <- function(x) isTRUE(x > 0)
  f <- firmly(function(x, y) NULL, is_positive ~ quos(x, y, x - y))
  expect_error(f(2, 1), NA)
  expect_error(f(0, 1), errmsg_false("is_positive(x)"))
  expect_error(f(0, 1), errmsg_false("is_positive(x - y)"))
})

# Lambda predicate function -----------------------------------------------
context("Lambda predicate function")

test_that("global lambda expression is interpreted as a predicate function", {
  z <- 0
  f <- firmly(function(x, y) NULL, {. > z})
  expect_error(f(1, 1), NA)
  expect_error(f(0), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
})

test_that("local lambda expression is interpreted as predicate function", {
  z <- 0
  f <- firmly(function(x, y) NULL, {. > z} ~ x)
  expect_error(f(1, stop("!")), NA)
  expect_error(f(0), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
})

# Quasiquotation ----------------------------------------------------------
context("Quasiquotation")

test_that("global-check predicate supports quasiquotation", {
  predicate1 <- quote(function(x) x > 0)
  predicate2 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  zero <- 0
  f <- firmly(function(x, y) NULL, !! predicate1, !! predicate2, {. > !!zero})
  expect_error(f(1, 1), NA)
  expect_error(f(0, 1), errmsg_false("(function(x) x > 0)(x)"), perl = TRUE)
  expect_error(f(0, 1), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
  expect_error(f(0, 1), errmsg_false("(function(.) {. > 0})(x)"), perl = TRUE)
})

test_that("local-check predicate supports quasiquotation", {
  predicate1 <- quote(function(x) x > 0)
  predicate2 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  zero <- 0
  f <- firmly(function(x, y) NULL,
              UQ(predicate1) ~ x, {. > !!zero} ~ x, UQ(predicate2) ~ y)
  expect_error(f(1, 1), NA)
  expect_error(f(0, 0), errmsg_false("(function(x) x > 0)(x)"), perl = TRUE)
  expect_error(f(0, 0), errmsg_false("(function(.) {. > 0})(x)"), perl = TRUE)
  expect_error(f(0, 0), errmsg_false("(function(.) {. > z})(y)"), perl = TRUE)
})

test_that("check items support quasiquotation", {
  q <- local({
    one <- 1
    rlang::quo(y - one)
  })
  two <- 2
  f <- firmly(function(x, y) NULL, {. > 0} ~ quos(x - !!two, !!q))
  expect_error(f(3, 2), NA)
  expect_error(f(2, 1),
               errmsg_false("(function(.) {. > 0})(x - 2)"), perl = TRUE)
  expect_error(f(2, 1),
               errmsg_false("(function(.) {. > 0})(y - one)"), perl = TRUE)
})

test_that("error message for global check supports quasiquotation", {
  msg <- "'{{.}}' is not true: {.}"
  f <- firmly(function(x, y) NULL, !!msg := isTRUE)
  expect_error(f(TRUE, TRUE), NA)
  expect_error(f("indeed not", TRUE), "'x' is not true: indeed not")
})

test_that("error message local to check item supports quasiquotation", {
  msg <- "'x' is not true: {x}"
  f <- firmly(function(x, y) NULL, isTRUE ~ quos(!!msg := x))
  expect_error(f(TRUE), NA)
  expect_error(f("indeed not"), "'x' is not true: indeed not")
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

test_that("input validation is evaluated in environment of check item", {
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
  f <- firmly(function(x, y) NULL, isTRUE)
  expect_error(f(x = FALSE), errmsg_false("isTRUE(x)"))
  expect_error(f(y = FALSE), errmsg_false("isTRUE(y)"))
})

test_that("unnamed local check uses auto-generated error messages", {
  f <- firmly(function(x, y) NULL, isTRUE ~ quos(x, y))
  expect_error(f(FALSE, TRUE), errmsg_false("isTRUE(x)"))
  expect_error(f(TRUE, FALSE), errmsg_false("isTRUE(y)"))
})

test_that("named global check uses name as error message", {
  f <- firmly(function(x, y) NULL, "Not true" = isTRUE)
  expect_n_errors(1, f, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(2, f, list(x = FALSE, y = FALSE), "Not true")
})

test_that("named local check uses name as error message", {
  f <- firmly(function(x, y) NULL, "Not true" = isTRUE ~ quos(x, y))
  expect_n_errors(1, f, list(x = FALSE, y = TRUE), "Not true")
  expect_n_errors(1, f, list(x = TRUE, y = FALSE), "Not true")
  expect_n_errors(2, f, list(x = FALSE, y = FALSE), "Not true")
})

test_that("name of local expression overrides name of check", {
  f <- firmly(function(x, y) NULL, "global" = isTRUE ~ quos("local" = x, y))
  expect_error(f(FALSE, TRUE), "local")
  expect_error(f(TRUE, FALSE), "global")
})

test_that("name of local expression overrides auto-generated error message", {
  f <- firmly(function(x, y) NULL, isTRUE ~ quos("local" = x, y))
  expect_error(f(FALSE, TRUE), "local")
  expect_error(f(TRUE, FALSE), errmsg_false("isTRUE(y)"))
})

test_that("auto-message is used if error message fails to be created", {
  f <- firmly(function(x, y) NULL, "{stop('!')}" = isTRUE ~ x)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))
  expect_error(f(FALSE), "Error interpolating message")
})

context("String-interpolation of error messages")

test_that("error messages of named global check interpolate dot", {
  f <- local({
    s_quote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      function(x, y) NULL,
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

test_that("generated error message is interpolated in check environment", {
  chk <- local({
    bar <- function(x) sprintf("local text: %s", x)
    rlang::quos("{{bar(.)}}; value: {.}" = isTRUE)
  })
  bar <- function(x) "this should not have been called"
  f <- firmly(function(x) NULL, checklist = chk)
  expect_error(f("not true"), "local text: x; value: not true")
})

test_that("error messages of named local check interpolate dot", {
  f <- local({
    s_quote <- function(x) encodeString(x, quote = "'")
    is_scalar <- function(x) length(x) == 1L
    firmly(
      function(x, y) NULL,
      "{{s_quote(.)}} is not a scalar (length is {length(.)})" = is_scalar ~
        quos(x, x - y)
    )
  })
  expect_error(f(1:3), esc_perl("'x' is not a scalar (length is 3)"))
  expect_error(f(1:2, 1:2),
               esc_perl("'x' is not a scalar (length is 2)"))
  expect_error(f(1:2, 1:2),
               esc_perl("'x - y' is not a scalar (length is 2)"))
})

test_that("error messages of unnamed global check don't interpolate dot", {
  f <- firmly(function(x, y) NULL, {.})
  expect_error(f(FALSE, TRUE), errmsg_false("(function(.) {.})(x)"))
  expect_error(f(TRUE, FALSE), errmsg_false("(function(.) {.})(y)"))
})

test_that("error messages of unnamed local check don't interpolate dot", {
  f <- firmly(function(x, y) NULL, {.} ~ quos(x))
  expect_error(f(FALSE, TRUE), errmsg_false("(function(.) {.})(x)"))
  expect_error(f(TRUE, FALSE), NA)
})

test_that("name of check item is interpolated in check-item scope", {
  chk <- local({
    a <- "local"
    rlang::quos("x is {x}, a is {a}, y is {y}" = x)
  })
  f <- firmly(function(x, y) NULL, isTRUE ~ !! chk)
  expect_error(f(FALSE, "y"), "x is FALSE, a is local, y is y")
})
