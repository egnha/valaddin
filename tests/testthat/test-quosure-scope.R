context("Check-scope conversion")

context("Localization")

# Input validation of localize()
test_that("only the first predicate supplied is processesed", {
  chkr1 <- localize(isTRUE)
  chkr2 <- suppressWarnings(localize(isTRUE, y))
  expect_equal(chkr1, chkr2)
})

test_that("warning is raised when more than one argument is supplied", {
  warn_msg <- "Only the first predicate will be localized"
  expect_warning(localize(isTRUE, y), warn_msg)
  expect_warning(tryCatch(localize(x, y), error = function(e) NULL), warn_msg)
})

test_that("error is raised when argument is not a function/lambda expression", {
  err_msg <- "Not a function"
  fake_predicates <- list(NULL, NA, 1:2, "a", mtcars, list(ls))
  for (x in fake_predicates) {
    expect_error(localize(x), err_msg)
  }
})

# Return value
test_that("localized function is an object of class 'check_maker'", {
  expect_true("check_maker" %in% class(localize(isTRUE)))
})

test_that("localized lambda expression is an object of class 'check_maker'", {
  expect_true("check_maker" %in% class(localize({isTRUE(. > 0)})))
})

# Scope of input validation
test_that("localized predicate uses its scope for validation", {
  chk_is_inside <- local({
    inside <- "inside"
    localize(function(x) identical(x, inside))
  })
  f <- firmly(function(x) NULL, chk_is_inside(x))
  expect_error(f("inside"), NA)
  expect_error(f(""), errmsg_false("(function(x) identical(x, inside))(x)"))
})

test_that("check items of local check maker use their scope for validation", {
  chk_equals_inside_z <- local({
    z <- 0
    equals_inside_z <- function(x) isTRUE(all.equal(x, z))
    localize(equals_inside_z)
  })
  z <- 1
  f <- firmly(function(x) NULL, chk_equals_inside_z(x - z))
  expect_error(f(1), NA)
  expect_error(f(0), errmsg_false("equals_inside_z(x - z)"))
})

# Error messages
test_that("unnamed localized predicate as check has default error message", {
  chk_is_true <- localize(isTRUE)
  f <- firmly(function(x) NULL, chk_is_true(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))
})

test_that("named localized predicate as check has name as error message", {
  chk_is_true <- localize("Not true" = isTRUE)
  f <- firmly(function(x) NULL, chk_is_true(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), "Not true")
})

test_that("local error message overrides that of localized predicate", {
  chk_is_true <- localize("Not true" = isTRUE)
  f <- firmly(function(x) NULL, chk_is_true("x is not true: {x}" = x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), "x is not true: FALSE")
})

# Quasiquotation
test_that("localization supports quasiquotation of argument", {
  z <- local({z <- 0; rlang::quo(z)})
  chkr1 <- localize(function(x) isTRUE(x > !!z))
  chkr2 <- localize(function(x) isTRUE(x > 0))
  expect_equal(localize(chkr1), localize(chkr2))
})

test_that("localization supports unquoting of error message", {
  msg <- "{{toupper(.)}} is not true: {.}"
  chk_is_true <- localize(!!msg := isTRUE)
  f <- firmly(function(x) NULL, chk_is_true(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), "X is not true: FALSE")
})
