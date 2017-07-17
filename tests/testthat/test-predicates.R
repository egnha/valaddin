context("Predicates")

test_that("pred_function() gets predicate for local predicates", {
  expect_identical(pred_function(localize(isTRUE)), isTRUE)
  expect_identical(pred_function(localize(isTRUE, "message")), isTRUE)
})

test_that("pred_function() gets predicate for global predicates", {
  lcl <- localize(isTRUE)
  expect_identical(pred_function(globalize(lcl)), isTRUE)
})

test_that("pred_message() gets predicate for local predicates", {
  expect_identical(pred_message(localize(isTRUE, "message")), "message")
  expect_identical(pred_message(localize(isTRUE)), "")
})

test_that("pred_message() gets predicate for global predicates", {
  expect_identical(pred_message(globalize(localize(isTRUE))), "")
  expect_identical(
    pred_message(globalize(localize(isTRUE, "message"))),
    "message"
  )
})

test_that("`pred_message<-()` reassigns message", {
  lcl <- localize(isTRUE, "old")
  pred_message(lcl) <- "new"
  gbl <- globalize(localize(isTRUE, "old"))
  pred_message(gbl) <- "new"
  expect_identical(pred_message(lcl), "new")
  expect_identical(pred_message(gbl), "new")
})
