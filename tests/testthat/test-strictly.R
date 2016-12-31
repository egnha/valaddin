context("Strictly")

test_that("function is unchanged if no checks given", {
  for (args in args_list) {
    pass_args(args) %>% expect_identical(strictly(.), .)
  }
})

test_that("argument signature is preserved", {
  for (args in args_list) {
    expect_identical(formals(strictly(make_fnc(args))), as.pairlist(args))
  }
})

test_that("original body, environment, and attributes are preserved", {
  set.seed(1)

  core <- function(f) {
    if (length(nomen(formals(f))$nm)) sc_core(f) else body(f)
  }
  core_attributes <- function(f) {
    sc_attr <- c("class", "..sc_core..", "..sc_check..", "..sc_arg_req..")
    attributes(f)[setdiff(names(attributes(f)), sc_attr)]
  }
  len <- sample(100L, length(args_list))
  for (i in seq_along(args_list)) {
    junk <- paste(sample(letters, len[[i]], replace = TRUE))
    body <- substitute(quote(x), list(x = junk))
    attr <- setNames(as.list(sample(LETTERS)), letters)
    env <- new.env()
    f <- do.call("structure",
      c(.Data = make_fnc(args_list[[i]], body, env), attr)
    )
    f_strict1 <- strictly(f, list(~x) ~ is.numeric)
    f_strict2 <- strictly(f_strict1, .warn_missing = TRUE)

    expect_identical(core(f_strict1), body(f))
    expect_identical(core(f_strict2), body(f))
    expect_identical(environment(f_strict1), environment(f))
    expect_identical(environment(f_strict2), environment(f))
    expect_identical(core_attributes(f_strict1), attributes(f))
    expect_identical(core_attributes(f_strict2), attributes(f))
    expect_identical(
      class(f_strict1)[class(f_strict1) != "strict_closure"],
      class(f)
    )
    expect_identical(
      class(f_strict2)[class(f_strict1) != "strict_closure"],
      class(f)
    )
  }
})

test_that("checks in '...' are combined with .checklist", {
  sorted_sc_check <- function(..f) {
    calls <- sc_check(..f)
    calls[sort(names(calls))]
  }

  f <- function(x, y = x, z = 0, ...) NULL
  chk1 <- list(~x) ~ {. > 0}
  chk2 <- ~is.numeric

  f_strict <- strictly(f, chk1, chk2)
  calls <- sc_check(f_strict)
  # 4 checks: One global check on 3 arguments, plus a check on 1 argument
  expect_identical(length(calls), 4L)

  f_strict2 <- strictly(f, chk1, .checklist = list(chk2))
  expect_identical(sorted_sc_check(f_strict2), calls)

  f_strict3 <- strictly(f, .checklist = list(chk1, chk2))
  expect_identical(sorted_sc_check(f_strict3), calls)
})

test_that("existing checks are preserved when adding new checks", {})

test_that(".warn_message = FALSE always removes missing argument check", {})

test_that(".warn_message = TRUE always adds missing argument check", {})

test_that("error is raised if function not a closure", {})

test_that("error is raised if .warning_message is not NULL or logical", {})

test_that("error is raised if checks form invalid checklist", {})