context("firmly()")

test_that("error raised when f is not a function", {
  non_fn <- list(NULL, NA, 0, letters, character(0), mtcars, list(log))
  for (x in non_fn) {
    expect_error(firmly(x), "'f' must be a function")
  }
  # But no error if f is a function
  expect_error(firmly(log), NA)       # special
  expect_error(firmly(sin), NA)       # builtin
  expect_error(firmly(identity), NA)  # closure
})

test_that("error raised when error_class is not NULL or a character vector", {
  f <- function(x) NULL
  expect_error(firmly(f, error_class = NULL), NA)
  expect_error(firmly(f, error_class = character()), NA)
  expect_error(firmly(f, error_class = letters), NA)

  ff <- firmly(f, isTRUE)
  expect_error(firmly(ff, error_class = NULL), NA)
  expect_error(firmly(ff, error_class = character()), NA)
  expect_error(firmly(ff, error_class = letters), NA)

  non_chr <- list(NA, 0, log, mtcars)
  for (x in non_chr) {
    expect_error(firmly(f, error_class = x),
                 "'error_class' must be NULL or a character vector without NAs")
  }
})

test_that("error raised when error_class contains an NA", {
  f <- function(x) NULL
  na_vec <- list(NA_character_, c("a", NA_character_))
  for (x in na_vec) {
    expect_error(firmly(f, error_class = x),
                 "'error_class' must be NULL or a character vector without NAs")
  }
})

test_that("f is returned when it has no named arguments", {
  fs <- list(function() NULL, function(...) NULL)
  for (f in fs) {
    expect_identical(firmly(f), f)
    expect_identical(firmly(f, is.numeric), f)
    expect_identical(firmly(f, error_class = "myClass"), f)
    expect_identical(firmly(f, is.numeric, error_class = "myClass"), f)
  }
})

test_that("f is returned when no checks are given and f itself has no checks", {
  f <- function(x) NULL
  expect_identical(firmly(f), f)
  expect_identical(firmly(f, error_class = "myClass"), f)
})

test_that("f is returned when no checks are given and error_class is void", {
  # "void" means character(0) or vector of empty strings
  f <- function(x) NULL
  expect_identical(firmly(f, error_class = character(0)), f)
  expect_identical(firmly(f, error_class = c("", "")), f)
})

test_that("firmly(f) has class 'firm_closure', inheriting from class(f)", {
  f <- function(x) NULL
  class(f) <- c("myClass", class(f))
  expect_identical(class(firmly(f, is.numeric)), c("firm_closure", class(f)))
  expect_identical(class(firmly(f, is.numeric, error_class = "x")),
                   c("firm_closure", class(f)))
})

test_that("firmly(f) has the same attributes as f, aside from its class", {
  f <- function(x) NULL
  class(f) <- c("firm_closure", class(f))
  expect_identical(attributes(firmly(f, is.numeric)), attributes(f))
  expect_identical(attributes(firmly(f, is.numeric, error_class = "x")),
                   attributes(f))
})

test_that("error raised if predicate is not a function or lambda expression", {
  f <- function(x) NULL
  # lambda expressions are OK
  expect_error(firmly(f, {isTRUE(.)}), NA)
  # but other non-functions are not OK
  fake_predicates <- list(NULL, NA, 1:2, mtcars, list(ls), quote(isTRUE))
  for (fake_pred in fake_predicates)
    expect_error(firmly(f, fake_pred), "Not a function")
})
