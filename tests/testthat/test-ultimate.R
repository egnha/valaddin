context("Ultimate validation syntax")

f <- function(x, y) NULL

false_x <- only(errmsg_false("isTRUE(x)"), not = errmsg_false("isTRUE(y)"))
false_y <- only(errmsg_false("isTRUE(y)"), not = errmsg_false("isTRUE(x)"))
false_xy <- both(errmsg_false("isTRUE(x)"), errmsg_false("isTRUE(y)"))

test_that("global check is implemented by bare predicate", {
  foo <- firmly(f, isTRUE)
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), false_x)
  expect_error_perl(foo(TRUE, FALSE), false_y)
  expect_error_perl(foo(FALSE, FALSE), false_xy)

  bar <- firmly(f, base::isTRUE)
  expect_error(bar(TRUE, TRUE), NA)
  expect_error_perl(
    bar(FALSE, TRUE),
    only(errmsg_false("base::isTRUE(x)"), not = errmsg_false("base::isTRUE(y)"))
  )
  expect_error_perl(
    bar(TRUE, FALSE),
    only(errmsg_false("base::isTRUE(y)"), not = errmsg_false("base::isTRUE(x)"))
  )
  expect_error_perl(
    bar(FALSE, FALSE),
    both(errmsg_false("base::isTRUE(x)"), errmsg_false("base::isTRUE(y)"))
  )
})

test_that("global check is implemented by anonymous function", {
  foo <- firmly(f, {isTRUE(.)})
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(
    foo(FALSE, TRUE),
    only(errmsg_false("(function (.) {isTRUE(.)})(x)"), not = "isTRUE(y)")
  )
  expect_error_perl(
    foo(TRUE, FALSE),
    only(errmsg_false("(function (.) {isTRUE(.)})(y)"), not = "isTRUE(x)")
  )
  expect_error_perl(
    foo(FALSE, FALSE),
    both(
      errmsg_false("(function (.) {isTRUE(.)})(x)"),
      errmsg_false("(function (.) {isTRUE(.)})(y)")
    )
  )

  bar <- firmly(f, function(.) isTRUE(.))
  expect_error(bar(TRUE, TRUE), NA)
  expect_error_perl(
    bar(FALSE, TRUE),
    only(errmsg_false("(function(.) isTRUE(.))(x)"), not = "isTRUE(y)")
  )
  expect_error_perl(
    bar(TRUE, FALSE),
    only(errmsg_false("(function(.) isTRUE(.))(y)"), not = "isTRUE(x)")
  )
  expect_error_perl(
    bar(FALSE, FALSE),
    both(
      errmsg_false("(function(.) isTRUE(.))(x)"),
      errmsg_false("(function(.) isTRUE(.))(y)")
    )
  )
})

test_that("global check is implemented by empty predicate call", {
  foo <- firmly(f, isTRUE())
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), false_x)
  expect_error_perl(foo(TRUE, FALSE), false_y)
  expect_error_perl(foo(FALSE, FALSE), false_xy)
})

test_that("local checks are implemented as predicate arguments", {
  foo <- firmly(f, isTRUE(x))
  expect_error(foo(TRUE), NA)
  expect_error_perl(foo(FALSE), false_x)

  bar <- firmly(f, isTRUE(x, y))
  expect_error(bar(TRUE, TRUE), NA)
  expect_error_perl(bar(FALSE, TRUE), false_x)
  expect_error_perl(bar(TRUE, FALSE), false_y)
  expect_error_perl(bar(FALSE, FALSE), false_xy)
})

test_that("name of global check is error message", {
  msg <- "error message"
  foo <- firmly(f, "error message" := isTRUE())
  expect_error(foo(TRUE, TRUE), NA)
  expect_error(foo(FALSE, TRUE), msg)
  expect_error(foo(TRUE, FALSE), msg)
  expect_error(foo(FALSE, FALSE), msg)
})

test_that("name of local check is error message", {
  msg <- "error message"

  foo <- firmly(f, isTRUE("error message" := x, y))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error(foo(FALSE, TRUE), msg)
  expect_error_perl(foo(TRUE, FALSE),
                    only(errmsg_false("isTRUE(y)"), not = msg))
  expect_error_perl(foo(FALSE, FALSE),
                    both(msg, errmsg_false("isTRUE(y)")))

  bar <- firmly(f, "global" := isTRUE("local" := x, y))
  expect_error(bar(TRUE, TRUE), NA)
  expect_error_perl(bar(FALSE, TRUE), only("local", not = "global"))
  expect_error_perl(bar(TRUE, FALSE), only("global", not = "local"))
  expect_error_perl(bar(FALSE, FALSE), both("local", "global"))
})
