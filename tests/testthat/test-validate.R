context("Object validation")

test_that("object is returned invisibly when validation passes", {
  # valid object is returned ...
  expect_identical(
    validate(mtcars,
             is.data.frame,
             {nrow(.) > 1},
             all ~ c("mpg", "cyl") %in% colnames(.)),
    mtcars
  )
  # ... invisibly
  expect_identical(
    capture_output(
      validate(mtcars,
               is.data.frame,
               {nrow(.) > 1},
               all ~ c("mpg", "cyl") %in% colnames(.))
    ),
    ""
  )
})

test_that("error listing violations is signaled when validation fails", {
  n <- nrow(mtcars)
  expect_error(
    validate(mtcars,
             is.matrix,
             "Not enough rows: {.}" := {. > n + 1} ~ nrow(.),
             {all(c("mpg", "cyl") %in% colnames(.))}),
    errmsg_false("is.matrix(.)")
  )
  expect_error(
    validate(mtcars,
             is.matrix,
             "Not enough rows: {.}" := {. > n + 1} ~ nrow(.),
             {all(c("mpg", "cyl") %in% colnames(.))}),
    paste("Not enough rows:", nrow(mtcars))
  )
})

test_that("validify() creates object validator", {
  n <- nrow(mtcars)
  verify_fail <- validify(
    is.matrix,
    "Not enough rows: {.}" := {. > n + 1} ~ nrow(.),
    {all(c("mpg", "cyl") %in% colnames(.))}
  )
  expect_error(
    verify_fail(mtcars),
    errmsg_false("is.matrix(.)")
  )
  expect_error(
    verify_fail(mtcars),
    paste("Not enough rows:", n)
  )

  verify_pass <- validify(
    is.data.frame,
    {nrow(.) > 1},
    all ~ c("mpg", "cyl") %in% colnames(.)
  )
  # valid object is returned ...
  expect_identical(verify_pass(mtcars), mtcars)
  # ... invisibly
  expect_identical(capture_output(verify_pass(mtcars)), "")
})
