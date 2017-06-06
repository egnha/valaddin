context("loosely()")

test_that("error raised when f is not a function", {
  non_closure <- list(NULL, NA, 0, letters, character(0), mtcars, list())
  for (x in non_closure) {
    expect_error(loosely(x), "'f' must be a function")
  }
})

test_that("f is returned unchanged when it is not a firm closure", {
  fs <- list(log, ls, function() NULL, function(...) NULL)
  for (f in fs) {
    expect_identical(loosely(f), f)
  }
})

test_that("original function is returned", {
  fs <- list(ls, function(x) NULL, function() NULL, function(...) NULL)
  for (f in fs) {
    expect_identical(loosely(firmly(f, isTRUE)), f)
  }
})

test_that("original function environment is restored", {
  f <- local(function(x) NULL)
  ff <- firmly(f, isTRUE)
  expect_identical(environment(loosely(ff)), environment(f))
})

test_that("original function attributes are restored", {
  attr <- {set.seed(1); setNames(as.list(sample(LETTERS)), letters)}
  f <- do.call("structure", c(.Data = function(x) NULL, attr))
  ff <- firmly(f, isTRUE)
  expect_identical(attributes(loosely(ff)), attributes(f))
})
