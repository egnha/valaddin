context("firmly()")

test_that("error raised when f is not a closure", {
  non_closure <- list(NULL, NA, 0, letters, character(0), mtcars, log, list())
  for (x in non_closure) {
    expect_error(firmly(x), "'f' must be a closure")
  }
})

test_that("error raised when checklist is not a list of quosures", {
  f <- function(x) NULL
  non_quos <- list(rlang::quo(isTRUE), list(rlang::quos(isTRUE)), list(~isTRUE))
  for (x in non_quos) {
    expect_error(firmly(x, checklist = x),
                 "'checklist' must be a list of quosures")
  }
})

test_that("error raised when error_class is not a character vector", {
  f <- function(x) NULL
  non_chr <- list(NULL, NA, 0, log, mtcars)
  for (x in non_chr) {
    expect_error(firmly(f, error_class = x),
                 "'error_class' must be a character vector without NAs")
  }
})

test_that("error raised when error_class contains an NA", {
  f <- function(x) NULL
  na_vec <- list(NA_character_, c("a", NA_character_))
  for (x in na_vec) {
    expect_error(firmly(f, error_class = x),
                 "'error_class' must be a character vector without NAs")
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
