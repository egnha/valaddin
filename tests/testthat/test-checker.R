context("Checker")

context("Localization")

test_that("error is raised when argument is not a function/lambda expression", {
  err_msg <- "Not a function"
  fake_predicates <- list(NULL, NA, 1:2, "a", mtcars, list(ls), quote(isTRUE))
  for (x in fake_predicates) {
    expect_error(checker(x), err_msg)
  }
})

# Scope of input validation
test_that("checker uses its scope for validation", {
  chk_is_inside <- local({
    inside <- "inside"
    checker("Not inside" := function(x) identical(x, inside))
  })
  f <- firmly(function(x) NULL, chk_is_inside(x))
  expect_error(f("inside"), NA)
  expect_error(f(""), "Not inside")
})

test_that("check items of checker use their scope for validation", {
  chk_equals_inside_z <- local({
    z <- 0
    equals_inside_z <- function(x) isTRUE(all.equal(x, z))
    checker(equals_inside_z)
  })
  z <- 1
  f <- firmly(function(x) NULL, chk_equals_inside_z(x - z))
  expect_error(f(z), NA)
  expect_error(f(0), errmsg_false("equals_inside_z(x - z)"))
})

# Error messages
test_that("checker with unnamed predicate uses default error message", {
  z <- 0
  f <- firmly(function(x) NULL, checker(isTRUE)(x))
  g <- firmly(function(x) NULL, checker({. > z})(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))
  expect_error(g(1), NA)
  expect_error_perl(g(0), errmsg_false("(function (.) {. > z})(x)"))
})

test_that("checker with named predicate uses name as error message", {
  f <- firmly(function(x) NULL, checker("Not true" := isTRUE)(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), "Not true")
})

test_that("local error message overrides that of checker", {
  chk_is_true <- checker("Not true" := isTRUE)

  f <- firmly(
    function(x) NULL,
    chk_is_true("x is not true: {x}" := x)
  )
  expect_error(f(TRUE), NA)
  expect_error(f("indeed not"), "x is not true: indeed not")

  f <- firmly(
    function(x) NULL,
    "Won't override" := chk_is_true("x is not true: {x}" := x)
  )
  expect_error(f(TRUE), NA)
  expect_error(f("indeed not"), "x is not true: indeed not")
})

test_that("name of checker overrides checker error message", {
  chk_is_true <- checker("Not true" := isTRUE)
  f <- firmly(function(x) NULL, "Will override" := chk_is_true(x))
  expect_error(f(TRUE), NA)
  expect_error(f("indeed not"), "Will override")
})

# Quasiquotation
test_that("checker supports quasiquotation of argument", {
  z <- 0
  predicate <- function(x) isTRUE(x > 0)
  chkr1 <- checker(function(x) isTRUE(x > !! z))
  chkr2 <- checker(function(x) isTRUE(x > 0))
  chkr3 <- checker(!! predicate)
  f <- function(x) NULL
  expect_error(firmly(f, chkr1(x))(1), NA)
  expect_error(firmly(f, chkr2(x))(1), NA)
  expect_error(firmly(f, chkr3(x))(1), NA)
  expect_error_perl(
    firmly(f, chkr1(x))(0),
    errmsg_false("(function(x) isTRUE(x > 0))(x)")
  )
  expect_error_perl(
    firmly(f, chkr2(x))(0),
    errmsg_false("(function(x) isTRUE(x > 0))(x)")
  )
  expect_error_perl(
    firmly(f, chkr3(x))(0),
    errmsg_false("(function (x) isTRUE(x > 0))(x)")
  )
})

context("Globalization")

test_that("checker is globalized when called w/o function arguments", {
  foo <- function(x, y, z) NULL

  chk_true <- checker("{{.}} not true" := isTRUE)
  foo_true <- firmly(foo, chk_true())
  expect_error(foo_true(TRUE, TRUE, TRUE), NA)
  expect_error(foo_true(FALSE, TRUE, FALSE), "x not true")
  expect_error(foo_true(FALSE, TRUE, FALSE), "z not true")

  # parameterized predicate
  chk_long <- checker("{{.}} not long enough" := function(., n) length(.) >= n)
  foo_long <- firmly(foo, chk_long(n = 1))
  expect_error(foo_long(0, 0, 0), NA)
  expect_error(foo_long(NULL, 0, NULL), "x not long enough")
  expect_error(foo_long(NULL, 0, NULL), "z not long enough")
})

context("Getters and setters")

test_that("chkr_predicate() gets predicate function for local predicates", {
  expect_identical(chkr_predicate(checker(isTRUE)), isTRUE)
  expect_identical(chkr_predicate(checker("message" := isTRUE)), isTRUE)
})

test_that("chkr_message() gets predicate for local predicates", {
  expect_identical(chkr_message(checker("message" := isTRUE)), "message")
  expect_identical(chkr_message(checker(isTRUE)), "")
})

test_that("`chkr_message<-()` sets message", {
  lp <- checker("old" := isTRUE)
  chkr_message(lp) <- "new"
  expect_identical(chkr_message(lp), "new")
})

context("Parameterized checkers")

test_that("predicate arguments can be transformed", {
  ISTRUE <- isTRUE
  get_function <- function(x) get(toupper(x), mode = "function")
  chk <- checker("Nope" := function(., pred) pred(.), pred = get_function)

  expect_error(
    firmly(function(x) NULL, chk("bogus")),
    "object 'BOGUS' of mode 'function' was not found"
  )

  foo <- firmly(function(x) NULL, chk("istrue"))
  expect_error(foo(TRUE), NA)
  expect_error(foo(FALSE), "Nope")
})
