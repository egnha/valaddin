context("Strictly")

test_that("function is unchanged if no checks and .warn_missing is NULL", {
  for (args in args_list) {
    f <-  pass_args(args)

    expect_identical(strictly(f), f)
  }
})

test_that("function is unchanged if it has no named arguments", {
  fns <- list(function() NULL, function(...) NULL)
  chks <- list(~is.numeric, "Negative" ~ {. >= 0}, list(~dummy) ~ is.function)

  for (f in fns) {
    expect_identical(strictly(f, .checklist = chks), f)
    expect_identical(strictly(f, .warn_missing = TRUE), f)
    expect_identical(strictly(f, .checklist = chks, .warn_missing = TRUE), f)
  }
})

test_that("argument signature is preserved", {
  for (args in args_list) {
    f <- make_fnc(args)

    expect_identical(formals(f), as.pairlist(args))
    expect_identical(formals(strictly(f)), formals(f))
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

test_that("checks in ... are combined with .checklist", {
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

test_that("existing checks are preserved when adding new checks", {
  f0 <- function(x, y, ...) NULL
  chks <- list(
    "Not numeric" ~ is.numeric,
    list("y not nonzero" ~ y) ~ {. != 0}
  )
  f <- strictly(f0, .checklist = chks)
  chks_f <- strict_checks(f)

  new_chks <- list(
    "Not less than one" = list("Not less than one" ~ x) ~ {. < 1},
    "Not positive" = "Not positive" ~ {. > 0}
  )

  for (err_msg in names(new_chks)) {
    chk <- new_chks[[err_msg]]

    g <- strictly(f, chk)
    chks_g <- strict_checks(g)

    # Checks of f subset of checks of g
    chks_f_g <- dplyr::distinct(dplyr::bind_rows(chks_g, chks_f))
    expect_identical(
      chks_f_g %>% dplyr::arrange_("string"),
      chks_g %>% dplyr::arrange_("string")
    )

    # All previous checks checked
    expect_error(g("1", 1), "Not numeric: `x`")
    expect_error(g(1, "1"), "Not numeric: `y`")
    expect_error(g(1, 0), "y not nonzero")

    # New check checked
    expect_error(g(2, -1), err_msg)
  }
})

test_that(".warn_missing = TRUE adds missing argument check", {
  f <- function(x, y, z = 0) NULL
  f_warn  <- strictly(f, .warn_missing = TRUE)
  f_check <- strictly(f_warn, list("z not numeric" ~ z) ~ is.numeric)

  # Check applied
  expect_error(f_check(1, 1, "1"), "z not numeric")

  # No warning if all required arguments supplied
  expect_warning(f_warn(1, 1), NA)
  expect_warning(f_check(1, 1), NA)

  args <- list(
    "x, y" = list(),
    "y"    = list(x = 1),
    "x"    = list(y = 1)
  )

  for (nms in names(args)) {
    arg <- args[[nms]]

    expect_warning(out <- do.call(f_warn, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out, do.call(f, arg))

    # Warning behavior persists in presence of checks
    expect_warning(out <- do.call(f_check, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out, do.call(f, arg))
  }
})

test_that(".warn_missing = FALSE removes missing argument check", {})

test_that(".warn_missing = NULL preserves missing-argument-check behavior", {})

test_that("error raised if function not a closure", {})

test_that("error raised if .warn_missing neither NULL nor logical", {})

test_that("error raised if checks form invalid checklist", {})