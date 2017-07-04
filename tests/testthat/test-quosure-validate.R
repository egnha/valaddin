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
    capture.output(
      validate(mtcars,
               is.data.frame,
               {nrow(.) > 1},
               all ~ c("mpg", "cyl") %in% colnames(.))
    ),
    character(0)
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
    sprintf("Not enough rows: %d", nrow(mtcars))
  )
})
