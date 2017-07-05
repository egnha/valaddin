context("Predicates")

test_that("predicate_function() gets predicate for local predicates", {
  expect_identical(predicate_function(localize(isTRUE)), isTRUE)
  expect_identical(predicate_function(localize(isTRUE, "message")), isTRUE)
})

test_that("predicate_function() gets predicate for global predicates", {
  lcl <- localize(isTRUE)
  expect_identical(predicate_function(globalize(lcl)), isTRUE)
})

test_that("predicate_message() gets predicate for local predicates", {
  expect_identical(predicate_message(localize(isTRUE, "message")), "message")
  expect_identical(predicate_message(localize(isTRUE)), "")
})

test_that("predicate_message() gets predicate for global predicates", {
  expect_identical(predicate_message(globalize(localize(isTRUE))), "")
  expect_identical(
    predicate_message(globalize(localize(isTRUE, "message"))),
    "message"
  )
})

test_that("`predicate_message<-()` reassigns message", {
  lcl <- localize(isTRUE, "old")
  predicate_message(lcl) <- "new"
  gbl <- globalize(localize(isTRUE, "old"))
  predicate_message(gbl) <- "new"
  expect_identical(predicate_message(lcl), "new")
  expect_identical(predicate_message(gbl), "new")
})
