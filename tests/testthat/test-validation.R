context("Input validation")

# Global-scope check ------------------------------------------------------
context("Global check")

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
context("Local check")

test_that("check formula with non-vld() RHS checks whole RHS expression", {
  # check of a bare argument
  f <- firmly(function(x, y) NULL, isTRUE(x))
  expect_error(f(TRUE, stop("!")), NA)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))

  # check of an expression of arguments
  f <- firmly(function(x, y) NULL, isTRUE(length(x) == 1L))
  expect_error(f(1, stop("!")), NA)
  expect_error(f(1:2), errmsg_false("isTRUE(length(x) == 1L)"))
})

test_that("check formula with vld() RHS checks contained expressions", {
  is_positive <- function(x) isTRUE(x > 0)

  f <- firmly(function(x, y) NULL, is_positive(x, y, x - y))
  expect_error(f(2, 1), NA)
  expect_error(f(0, 1), errmsg_false("is_positive(x)"))
  expect_error(f(0, 1), errmsg_false("is_positive(x - y)"))

  check_items <- vld_exprs(x, y, x - y)
  f <- firmly(function(x, y) NULL, is_positive(!!! check_items))
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
  expect_error_perl(f(0), errmsg_false("(function (.) {. > z})(x)"))
})

test_that("local lambda expression is interpreted as a predicate function", {
  z <- 0
  f <- firmly(function(x, y) NULL, {. > z}(x))
  expect_error(f(1, stop("!")), NA)
  expect_error_perl(f(0), errmsg_false("(function (.) {. > z})(x)"))
})

# Quasiquotation ----------------------------------------------------------
context("Quasiquotation")

test_that("global predicate function supports quasiquotation", {
  zero <- 0
  pred1 <- rlang::quo(function(x) x > zero)
  pred2 <- local({
    z <- 0
    rlang::quo({. > z})
  })
  f <- firmly(function(x, y) NULL, !! pred1, {. > !! zero}, UQ(pred2))
  expect_error(f(1, 1), NA)
  expect_error_perl(f(0, 1), errmsg_false("(function(x) x > zero)(x)"))
  expect_error_perl(f(0, 1), errmsg_false("(function (.) {. > z})(x)"))
  expect_error_perl(f(0, 1), errmsg_false("(function (.) {. > 0})(x)"))
})

test_that("local predicate function supports quasiquotation", {
  zero <- 0
  predicate <- local({
    z <- 0
    function(x) x > z
  })
  f <- firmly(function(x, y) NULL, UQ(predicate)(y), {. > !! zero}(x))
  expect_error(f(1, 1), NA)
  expect_error_perl(f(0, 0), errmsg_false("(function (x) x > z)(y)"))
  expect_error_perl(f(0, 0), errmsg_false("(function (.) {. > 0})(x)"))
})

test_that("check items support quasiquotation", {
  q <- local({
    one <- 1
    rlang::quo(y - one)
  })
  two <- 2
  f <- firmly(function(x, y) NULL, {. > 0}(x - !! two, !! q))
  expect_error(f(3, 2), NA)
  expect_error_perl(f(2, 1), errmsg_false("(function (.) {. > 0})(x - 2)"))
  expect_error_perl(f(2, 1), errmsg_false("(function (.) {. > 0})(y - one)"))

  check_items <- vld_exprs(x - !! two, !! q)
  f <- firmly(function(x, y) NULL, {. > 0}(!!! check_items))
  expect_error(f(3, 2), NA)
  expect_error_perl(f(2, 1), errmsg_false("(function (.) {. > 0})(x - 2)"))
  expect_error_perl(f(2, 1), errmsg_false("(function (.) {. > 0})(y - one)"))
})

test_that("error message for global check supports quasiquotation", {
  msg <- "'{{.}}' is not true: {.}"
  f <- firmly(function(x, y) NULL, !! msg := isTRUE)
  expect_error(f(TRUE, TRUE), NA)
  expect_error(f("indeed not", TRUE), "'x' is not true: indeed not")
})

test_that("error message local to check item supports quasiquotation", {
  msg <- "'x' is not true: {x}"
  f <- firmly(function(x, y) NULL, isTRUE(!! msg := x))
  expect_error(f(TRUE), NA)
  expect_error(f("indeed not"), "'x' is not true: indeed not")
})

# Splicing checks ---------------------------------------------------------
context("Splicing checks")

test_that("checks can be spliced when wrapped by vld()", {
  chk_pos <- vld_spec("{{.}} is not positive" := {isTRUE(. > 0)})
  chk_len <- vld_exprs("Length is {length(x)}" := length(x))
  f <- firmly(log, is.numeric, !!! chk_pos, {. == 1}(!!! chk_len))
  expect_equal(f(1), 0)
  expect_error(f("1"), errmsg_false("is.numeric(x)"))
  expect_error(f(0), "x is not positive")
  expect_error(f(1:2), "x is not positive")
  expect_error(f(1:2), "Length is 2")
})

# Lexical scope of checks -------------------------------------------------
context("Lexical scope of checks")

test_that("global predicate function is evaluated in scope of check", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private)
  })
  private <- "not private"  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("is_private(x)"))
})

test_that("local predicate function is evaluated in scope of check item", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private(x))
  })
  private <- "not private"  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("is_private(x)"))
})

test_that("input validation is evaluated in environment of check item", {
  f <- local({
    private <- "private"
    predicate <- base::identical
    firmly(function(x) NULL, isTRUE(predicate(x, private)))
  })
  private <- "not private"         # private and predicate
  predicate <- function(...) TRUE  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("isTRUE(predicate(x, private))"))
})

test_that("check-item names don't clash with names in calling frame", {
  f <- local({
    private <- "private"
    firmly(function(x) NULL, isTRUE(identical(x, private)))
  })
  private <- "not private"  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f(private), errmsg_false("isTRUE(identical(x, private))"))
})

test_that("predicate function doesn't clash with names in calling frame", {
  f <- local({
    private <- "private"
    is_private <- function(.) identical(., private)
    firmly(function(x) NULL, is_private(x))
  })
  is_private <- function(...) FALSE  # should be ignored by f
  expect_error(f("private"), NA)
  expect_error(f("not private"), errmsg_false("is_private(x)"))
})

test_that("messages, predicate, check expressions are independently scoped", {
  msg <- local({
    f <- toupper
    message <- "x-message"
    new_vld_error_msg("{message} {{f(.)}}")
  })
  is_positive <- local({
    f <- isTRUE
    function(.) f(. > 0)
  })
  x <- local({
    f <- function(a) a - 1
    rlang::quo(f(x))
  })

  f <- function(a) a - 2
  message <- "y-message"

  bar <- function(x, y) NULL
  foo <- firmly(bar, !! msg := is_positive(!! x, "{message} {y}" := f(y)))

  expect_error(foo(2, 3), NA)
  expect_error_perl(foo(1, 3), only("x-message F\\(X\\)", "y-message 3"))
  expect_error_perl(foo(2, 2), only("y-message 2", "x-message F\\(X\\)"))
  expect_error_perl(foo(1, 2), both("x-message F\\(X\\)", "y-message 2"))
})

# Error class -------------------------------------------------------------
context("Error class")

err_class <- function(x) c(x, "error", "condition")

test_that("default error subclass is 'inputValidationError'", {
  expect_identical(
    tryCatch(firmly(identity, isTRUE)(0), error = class),
    err_class("inputValidationError")
  )
})

test_that("error subclass of input validation error is error_class", {
  f0 <- identity
  f1 <- firmly(f0, is.numeric, error_class = "myError")
  f2 <- firmly(f1, error_class = "myNewError")
  f3 <- firmly(f2, {isTRUE(. > 0)}, error_class = "myNewestError")
  expect_identical(tryCatch(f1("0"), error = class), err_class("myError"))
  expect_identical(tryCatch(f2("0"), error = class), err_class("myNewError"))
  expect_identical(tryCatch(f3(0), error = class), err_class("myNewestError"))
})
