context("Collecting validation checks")

f <- function(x, y) NULL

only_false <- function(this, not_this) {
  only(errmsg_false(this), errmsg_false(not_this))
}
both_false <- function(this, that) {
  both(errmsg_false(this), errmsg_false(that))
}

context("Bare checks")

test_that("symbol collected as global predicate", {
  foo <- firmly(f, !!! vld_checks(isTRUE))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))
  expect_error_perl(foo(TRUE, FALSE), only_false("isTRUE(y)", "isTRUE(x)"))
  expect_error_perl(foo(FALSE, FALSE), both_false("isTRUE(x)", "isTRUE(y)"))
})

test_that("namespace-qualified symbol collected as global predicate", {
  foo <- firmly(f, !!! vld_checks(base::isTRUE))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(
    foo(FALSE, TRUE),
    only_false("base::isTRUE(x)", "base::isTRUE(y)")
  )
  expect_error_perl(
    foo(TRUE, FALSE),
    only_false("base::isTRUE(y)", "base::isTRUE(x)")
  )
  expect_error_perl(
    foo(FALSE, FALSE),
    both_false("base::isTRUE(x)", "base::isTRUE(y)")
  )
})

test_that("function declaration collected as global predicate", {
  foo <- firmly(f, !!! vld_checks(function(x) isTRUE(x)))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(
    foo(FALSE, TRUE),
    only_false("(function(x) isTRUE(x))(x)", "(function(x) isTRUE(x))(y)")
  )
  expect_error_perl(
    foo(TRUE, FALSE),
    only_false("(function(x) isTRUE(x))(y)", "(function(x) isTRUE(x))(x)")
  )
  expect_error_perl(
    foo(FALSE, FALSE),
    both_false("(function(x) isTRUE(x))(x)", "(function(x) isTRUE(x))(y)")
  )
})

test_that("lambda-function collected as global predicate", {
  foo <- firmly(f, !!! vld_checks({isTRUE(.)}))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(
    foo(FALSE, TRUE),
    only_false("(function (.) {isTRUE(.)})(x)", "(function (.) {isTRUE(.)})(y)")
  )
  expect_error_perl(
    foo(TRUE, FALSE),
    only_false("(function (.) {isTRUE(.)})(y)", "(function (.) {isTRUE(.)})(x)")
  )
  expect_error_perl(
    foo(FALSE, FALSE),
    both_false("(function (.) {isTRUE(.)})(x)", "(function (.) {isTRUE(.)})(y)")
  )
})

test_that("call collected as local predicate", {
  foo <- firmly(f, !!! vld_checks(isTRUE(x)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))

  foo <- firmly(f, !!! vld_checks(isTRUE(x, y)))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))
  expect_error_perl(foo(TRUE, FALSE), only_false("isTRUE(y)", "isTRUE(x)"))
  expect_error_perl(foo(FALSE, FALSE), both_false("isTRUE(x)", "isTRUE(y)"))
})

test_that("unquoted (quosure) function collected as global predicate", {
  bar <- function(.) isTRUE(.)
  foo <- firmly(f, !!! vld_checks(!! bar))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(
    foo(FALSE, TRUE),
    only_false("(function (.) isTRUE(.))(x)", "(function (.) isTRUE(.))(y)")
  )
  expect_error_perl(
    foo(TRUE, FALSE),
    only_false("(function (.) isTRUE(.))(y)", "(function (.) isTRUE(.))(x)")
  )
  expect_error_perl(
    foo(FALSE, FALSE),
    both_false("(function (.) isTRUE(.))(x)", "(function (.) isTRUE(.))(y)")
  )

  bar <- rlang::quo(isTRUE)
  foo <- firmly(f, !!! vld_checks(!! bar))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))
  expect_error_perl(foo(TRUE, FALSE), only_false("isTRUE(y)", "isTRUE(x)"))
  expect_error_perl(foo(FALSE, FALSE), both_false("isTRUE(x)", "isTRUE(y)"))
})

test_that("unquoted (quosure) call collected as global predicate", {
  bar <- quote(isTRUE(x))
  foo <- firmly(f, !!! vld_checks(!! bar))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))

  bar <- rlang::quo(isTRUE(x))
  foo <- firmly(f, !!! vld_checks(!! bar))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))
})

context("Named checks")

test_that("named symbol collected as global predicate with error message", {
  foo <- firmly(f, !!! vld_checks("error {{.}}" := isTRUE))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only("error x", "error y"))
  expect_error_perl(foo(TRUE, FALSE), only("error y", "error x"))
  expect_error_perl(foo(FALSE, FALSE), both("error x", "error y"))
})

test_that("named call collected as local predicate with error message", {
  foo <- firmly(f, !!! vld_checks("error {{.}}" := isTRUE(x)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only("error x", "error y"))

  foo <- firmly(f, !!! vld_checks("error {{.}}" := isTRUE(x, y)))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only("error x", "error y"))
  expect_error_perl(foo(TRUE, FALSE), only("error y", "error x"))
  expect_error_perl(foo(FALSE, FALSE), both("error x", "error y"))
})

test_that("name of symbol can be unquoted", {
  msg <- local({
    message <- "error"
    rlang::new_quosure("{message} {{.}}")
  })
  foo <- firmly(f, !!! vld_checks(!! msg := isTRUE))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only("error x", "error y"))
  expect_error_perl(foo(TRUE, FALSE), only("error y", "error x"))
  expect_error_perl(foo(FALSE, FALSE), both("error x", "error y"))
})

test_that("name of call can be unquoted", {
  msg <- local({
    message <- "error"
    rlang::new_quosure("{message} {{.}}")
  })

  foo <- firmly(f, !!! vld_checks(!! msg := isTRUE(x)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only("error x", "error y"))

  foo <- firmly(f, !!! vld_checks(!! msg := isTRUE(x, y)))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only("error x", "error y"))
  expect_error_perl(foo(TRUE, FALSE), only("error y", "error x"))
  expect_error_perl(foo(FALSE, FALSE), both("error x", "error y"))
})

context("Bare expressions")

test_that("symbol collected as check expression", {
  foo <- firmly(f, isTRUE(!!! vld_exprs(x)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))

  foo <- firmly(f, isTRUE(!!! vld_exprs(x, y)))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))
  expect_error_perl(foo(TRUE, FALSE), only_false("isTRUE(y)", "isTRUE(x)"))
  expect_error_perl(foo(FALSE, FALSE), both_false("isTRUE(x)", "isTRUE(y)"))
})

test_that("unquoted (quosure) symbol collected as check expression", {
  sym <- quote(x)
  foo <- firmly(f, isTRUE(!!! vld_exprs(!! sym)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))

  sym <- rlang::quo(x)
  foo <- firmly(f, isTRUE(!!! vld_exprs(!! sym)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), only_false("isTRUE(x)", "isTRUE(y)"))
})

context("Named expressions")

test_that("named symbol collected as check expression with error message", {
  foo <- firmly(f, isTRUE(!!! vld_exprs("error x" := x)))
  expect_error_perl(foo(TRUE, FALSE), NA)
  expect_error_perl(foo(FALSE, TRUE), "error x")
})

test_that("name of symbol can be unquoted", {
  msg <- local({
    message <- "error"
    rlang::new_quosure("{message} {x}")
  })
  foo <- firmly(f, isTRUE(!!! vld_exprs(!! msg := x)))
  expect_error(foo(TRUE, TRUE), NA)
  expect_error_perl(foo("x", TRUE), "error x")
})


