context("Check-scope conversion")

context("Localization")

test_that("error is raised when argument is not a function/lambda expression", {
  err_msg <- "Not a function"
  fake_predicates <- list(NULL, NA, 1:2, "a", mtcars, list(ls), quote(isTRUE))
  for (x in fake_predicates) {
    expect_error(localize(x), err_msg)
  }
})

# Return value
test_that("localized function has class 'local_predicate'", {
  expect_true("local_predicate" %in% class(localize(isTRUE)))
})

test_that("localized lambda expression has class 'local_predicate'", {
  expect_true("local_predicate" %in% class(localize({isTRUE(. > 0)})))
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

test_that("check items of local predicate use their scope for validation", {
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
  z <- 0
  f <- firmly(function(x) NULL, localize(isTRUE)(x))
  g <- firmly(function(x) NULL, localize({. > z})(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), errmsg_false("isTRUE(x)"))
  expect_error(g(1), NA)
  expect_error(g(0), errmsg_false("(function(.) {. > z})(x)"), perl = TRUE)
})

test_that("named localized predicate as check has name as error message", {
  f <- firmly(function(x) NULL, localize(isTRUE, "Not true")(x))
  expect_error(f(TRUE), NA)
  expect_error(f(FALSE), "Not true")
})

test_that("local error message overrides that of localized predicate", {
  chk_is_true <- localize(isTRUE, "Not true")
  f <- firmly(function(x) NULL, chk_is_true("x is not true: {x}" := x))
  expect_error(f(TRUE), NA)
  expect_error(f("indeed not"), "x is not true: indeed not")
})

# Quasiquotation
test_that("localization supports quasiquotation of argument", {
  z <- local({z <- 0; rlang::quo(z)})
  predicate <- function(x) isTRUE(x > 0)
  chkr1 <- localize(function(x) isTRUE(x > !! z))
  chkr2 <- localize(function(x) isTRUE(x > 0))
  chkr3 <- localize(!! predicate)
  expect_equal(localize(chkr1), localize(chkr2))
  expect_equal(localize(chkr1), localize(chkr3))
})

context("Globalization")

test_that("error is raised when argument is not a local predicate", {
  fake_local_predicate <- list(
    NULL, NA, 1:2, "a", mtcars, list(ls),
    # not a closure
    log,
    # not of class 'local_predicate'
    unclass(localize(isTRUE))
  )
  for (x in fake_local_predicate) {
    expect_error(globalize(x), "'chkr' must be a local predicate")
  }
})

test_that("globalization preserves message of localized predicate", {
  chkr1 <- localize(isTRUE)
  chkr2 <- localize(isTRUE, "{{.}} is not true: {.}")
  f <- function(x) NULL
  f1 <- firmly(f, !! globalize(chkr1))
  f2 <- firmly(f, !! globalize(chkr2))
  expect_error(f1(TRUE), NA)
  expect_error(f2(TRUE), NA)
  expect_error(f1("indeed not"), errmsg_false("isTRUE(x)"))
  expect_error(f2("indeed not"), "x is not true: indeed not")
})

context("Local scope inversion")

test_that("globalize(localize(pred)) is check-equivalent to pred", {
  predicate <- local({
    z <- 0
    function(x) isTRUE(x > z)
  })
  f <- function(x) NULL
  ff <- firmly(f, !! globalize(localize(predicate)))
  ff_ref <- firmly(f, predicate)
  expect_error(ff(1), NA)
  expect_error(ff(0), errmsg_false("predicate(x)"))
  expect_equal(ff, ff_ref)
})

context("Localized comparisons")

test_that("error is raised when p is not a function/lambda expression", {
  err_msg <- "Not a function"
  fake_predicates <- list(NULL, NA, 1:2, "a", mtcars, list(ls), quote(isTRUE))
  for (x in fake_predicates) {
    expect_error(localize_comparison(x), err_msg)
  }
})

test_that("error is raised when msg is not a string or NULL", {
  err_msg <- "'msg' must be a string or NULL"
  fake_msg <- list(NA, c("a", "b"), list("a"), character(0), quote(a))
  for (x in fake_msg) {
    expect_error(localize_comparison(isTRUE, x), err_msg)
  }
})
