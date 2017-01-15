context("Strictly")

test_that("function is unchanged if no checks given, .warn_missing is NULL", {
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

  len <- sample(length(args_list))

  for (i in seq_along(args_list)) {
    junk <- paste(sample(letters, len[[i]], replace = TRUE), collapse = "")
    body <- substitute(quote(x), list(x = junk))
    attr <- setNames(as.list(sample(LETTERS)), letters)
    env <- new.env(parent = baseenv())
    f <- do.call("structure",
      c(.Data = make_fnc(args_list[[i]], body, env), attr)
    )
    f_strict1 <- strictly(f, list(~x) ~ is.numeric)
    f_strict2 <- strictly(f_strict1, .warn_missing = TRUE)

    # Same body
    nms <- nomen(args_list[[i]])$nm
    args <- setNames(as.list(rep(0, length(nms))), nms)
    expect_identical(do.call(f_strict1, args), junk)
    expect_identical(do.call(f_strict2, args), junk)

    # Same environment
    if (is.null(strict_core(f_strict1))) {
      expect_identical(environment(f_strict1), environment(f))
      expect_identical(environment(f_strict2), environment(f))
    } else {
      expect_identical(environment(strict_core(f_strict1)), environment(f))
      expect_identical(environment(strict_core(f_strict2)), environment(f))
    }

    # Same attributes
    expect_identical(
      attributes(f_strict1)[names(attributes(f_strict1)) != "class"],
      attributes(f)
    )
    expect_identical(
      attributes(f_strict2)[names(attributes(f_strict2)) != "class"],
      attributes(f)
    )
    expect_identical(
      class(f_strict1)[class(f_strict1) != "strict_closure"],
      class(f)
    )
    expect_identical(
      class(f_strict2)[class(f_strict2) != "strict_closure"],
      class(f)
    )
  }
})

test_that("checks in '...' are combined with .checklist", {
  sort_checks <- function(..f) {
    dplyr::arrange_(strict_checks(..f), ~string)
  }

  f <- function(x, y = x, z = 0, ...) NULL
  chk1 <- list(~x) ~ {. > 0}
  chk2 <- ~is.numeric

  f_strict <- strictly(f, chk1, chk2)
  calls <- dplyr::arrange_(strict_checks(f_strict), ~string)
  # 4 checks: One global check on 3 arguments, plus a check on 1 argument
  expect_identical(nrow(calls), 4L)

  f_strict2 <- strictly(f, chk1, .checklist = list(chk2))
  expect_identical(sort_checks(f_strict2), calls)

  f_strict3 <- strictly(f, .checklist = list(chk1, chk2))
  expect_identical(sort_checks(f_strict3), calls)
})

test_that("existing checks are preserved when adding new checks", {})

test_that(".warn_message = FALSE always removes missing argument check", {})

test_that(".warn_message = TRUE always adds missing argument check", {})

test_that(".warn_message = NULL preserves missing-argument-check behavior", {})

test_that(".logical_void_as = logical(0) leaves logical(0) as is", {})

test_that(".logical_void_as = TRUE coerces logical(0) to TRUE", {})

test_that(".logical_void_as = FALSE coerces logical(0) to FALSE", {})

test_that(".logical_void_as = NULL preserves void-logical behavior", {})

test_that("error raised if function not a closure", {})

test_that("error raised if .warning_message neither NULL nor logical", {})

test_that("error raised if .logical_void_as neither NULL nor logical(0|1)", {})

test_that("error raised if checks form invalid checklist", {})