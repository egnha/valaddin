context("Input validation")

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
  expect_error(f(1), errmsg_invalid("identity(x)", 1))
  expect_error(f(NA), errmsg_invalid("identity(x)", NA))
  expect_error(f(NULL), errmsg_invalid("identity(x)", NULL))
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
  zero <- 0
  predicate1 <- rlang::quo(function(x) x > zero)
  predicate2 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  f <- firmly(function(x, y) NULL,
              !! predicate1, {. > !! zero}, UQ(predicate2))
  expect_error(f(1, 1), NA)
  expect_error(f(0, 1), errmsg_false("(function(x) x > zero)(x)"), perl = TRUE)
  expect_error(f(0, 1), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
  expect_error(f(0, 1), errmsg_false("(function(.) {. > 0})(x)"), perl = TRUE)
})

test_that("local-check predicate supports quasiquotation", {
  zero <- 0
  predicate1 <- rlang::quo(function(x) x > zero)
  predicate2 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  f <- firmly(function(x, y) NULL,
              UQ(predicate1) ~ x, {. > !! zero} ~ x, UQ(predicate2) ~ y)
  expect_error(f(1, 1), NA)
  expect_error(f(0, 0), errmsg_false("(function(x) x > zero)(x)"), perl = TRUE)
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

test_that("local-check predicate is evaluated in environment of check", {
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

test_that("check-item names don't clash with names in calling frame", {
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
