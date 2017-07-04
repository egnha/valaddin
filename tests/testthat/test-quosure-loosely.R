context("loosely()")

test_that("error raised when f is not a function", {
  non_fs <- list(NULL, NA, 0, letters, character(0), mtcars, quote(ls))
  for (x in non_fs)
    expect_error(loosely(x), "'f' must be a function")
})

test_that("f is returned unchanged when it is not a firm closure", {
  fs <- list(log, ls, unclass(firmly(identity, is.numeric)))
  for (f in fs)
    expect_identical(loosely(f), f)
})

test_that("closure of the original function is returned", {
  # original function is a closure
  fs <- list(ls, function(x) NULL, function() NULL, function(...) NULL)
  for (f in fs)
    expect_identical(loosely(firmly(f, isTRUE)), f)
  # original function is primitive
  expect_identical(loosely(firmly(log, is.numeric)), rlang::as_closure(log))
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
