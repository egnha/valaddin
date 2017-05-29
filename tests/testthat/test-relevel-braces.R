context("Re-level curly braces")

expect_relevel <- function(x, ref) {
  expect_identical(relevel_braces(x), ref)
}

test_that("outer single/out double braces are doubled/simplified", {
  expect_relevel(".",
                 ".")
  expect_relevel("{{.}}",
                 "{.}")
  expect_relevel("{{}}",
                 "{}")
  expect_relevel("{.}",
                 "{{.}}")
  expect_relevel("{}",
                 "{{}}")
  expect_relevel("{{.}.}",
                 "{{{.}.}}")
  expect_relevel("{{}.}",
                 "{{{}.}}")
  expect_relevel("{.{.}}",
                 "{{.{.}}}")
  expect_relevel("{{{.}.}}",
                 "{{.}.}")
  expect_relevel("{{.{.}}}",
                 "{.{.}}")
  expect_relevel("{{.}{.}}",
                 "{{{.}{.}}}")
  expect_relevel(".{{{{{.}}}}{}}",
                 ".{{{{{{.}}}}{}}}")
  expect_relevel(".{{.}.}.{{.}}.{.{.}}.{.}",
                 ".{{{.}.}}.{.}.{{.{.}}}.{{.}}")
})
