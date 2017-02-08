context("Strictly")

test_that("error raised when .f is not a closure", {
  errmsg <- "`.f` not an interpreted function"
  bad_fns <- list(NULL, NA, log, 1, "A", quote(ls))

  for (f in bad_fns) {
    expect_error(strictly(f), errmsg)
  }
})

test_that("error raised when .f has an argument named _chks__", {
  errmsg <- "Can't apply strictly\\(\\) when `.f` has an argument named `_chks__`"
  f <- function(`_chks__`, ...) NULL

  expect_error(strictly(f, .warn_missing = TRUE), errmsg)
  expect_error(strictly(f, ~ is.numeric), errmsg)
  expect_error(strictly(f, ~ is.numeric, .warn_missing = TRUE), errmsg)
})

test_that("error raised when .warn_missing is not NULL/TRUE/FALSE", {
  f <- function(x) NULL

  # No error if .warn_missing NULL/TRUE/FALSE
  expect_error(strictly(f, .warn_missing = TRUE), NA)
  expect_error(strictly(f, .warn_missing = FALSE), NA)
  expect_error(strictly(f, .warn_missing = NULL), NA)

  # Otherwise, error
  errmsg <- "`.warn_missing` neither NULL nor logical scalar"
  bad_val <- list(NA, logical(0), logical(2), 1, 0, "TRUE", "FALSE")
  for (val in bad_val) {
    expect_error(strictly(function(x) NULL, .warn_missing = val), errmsg)
  }
})

test_that("error raised when .checklist is an invalid checklist", {
  f <- function(x, y = 1, ...) NULL

  errmsg <- "Invalid argument checks"

  for (chk in bad_checks) {
    expect_error(strictly(f, chk), errmsg)
    expect_error(strictly(f, .checklist = list(chk)), errmsg)
    expect_error(strictly(f, ~{. > 0}, .checklist = list(chk)), errmsg)
    expect_error(strictly(f, chk, .checklist = list(~{. > 0})), errmsg)
    expect_error(strictly(f, .checklist = list(~{. > 0}, chk)), errmsg)

    expect_error(strictly_(f, chk), errmsg)
    expect_error(strictly_(f, .checklist = list(chk)), errmsg)
    expect_error(strictly_(f, ~{. > 0}, .checklist = list(chk)), errmsg)
    expect_error(strictly_(f, chk, .checklist = list(~{. > 0})), errmsg)
    expect_error(strictly_(f, .checklist = list(~{. > 0}, chk)), errmsg)
  }
})

test_that("function is unchanged when no checks and .warn_missing is NULL", {
  for (args in args_list) {
    f <-  pass_args(args)

    expect_identical(strictly(f), f)
    expect_identical(strictly_(f), f)
  }
})

test_that("function is unchanged when it has no named arguments", {
  fns <- list(function() NULL, function(...) NULL)
  chks <- list(~is.numeric, "Negative" ~ {. >= 0}, list(~dummy) ~ is.function)

  for (f in fns) {
    expect_identical(strictly(f, .checklist = chks), f)
    expect_identical(strictly(f, .warn_missing = TRUE), f)
    expect_identical(strictly(f, .warn_missing = FALSE), f)
    expect_identical(strictly(f, .checklist = chks, .warn_missing = TRUE), f)
    expect_identical(strictly(f, .checklist = chks, .warn_missing = FALSE), f)

    expect_identical(strictly_(f, .checklist = chks), f)
    expect_identical(strictly_(f, .warn_missing = TRUE), f)
    expect_identical(strictly_(f, .warn_missing = FALSE), f)
    expect_identical(strictly_(f, .checklist = chks, .warn_missing = TRUE), f)
    expect_identical(strictly_(f, .checklist = chks, .warn_missing = FALSE), f)
  }
})

test_that("argument signature is preserved", {
  for (args in args_list) {
    f <- make_fnc(args)

    expect_identical(formals(f), as.pairlist(args))
    expect_identical(formals(strictly(f)), formals(f))
    expect_identical(formals(strictly_(f)), formals(f))
  }
})

test_that("original function body/environment/attributes are preserved", {
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

    f_strict1_ <- strictly_(f, list(~x) ~ is.numeric)
    f_strict2_ <- strictly_(f_strict1_, .warn_missing = TRUE)

    # Same body
    nms <- nomen(args_list[[i]])$nm
    args <- setNames(as.list(rep(0, length(nms))), nms)
    expect_identical(do.call(f_strict1, args), junk)
    expect_identical(do.call(f_strict2, args), junk)
    expect_identical(do.call(f_strict1_, args), junk)
    expect_identical(do.call(f_strict2_, args), junk)

    # Same environment
    if (is.null(strict_core(f_strict1))) {
      expect_identical(environment(f_strict1), environment(f))
      expect_identical(environment(f_strict2), environment(f))
      expect_identical(environment(f_strict1_), environment(f))
      expect_identical(environment(f_strict2_), environment(f))
    } else {
      expect_identical(environment(strict_core(f_strict1)), environment(f))
      expect_identical(environment(strict_core(f_strict2)), environment(f))
      expect_identical(environment(strict_core(f_strict1_)), environment(f))
      expect_identical(environment(strict_core(f_strict2_)), environment(f))
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

    expect_identical(
      attributes(f_strict1_)[names(attributes(f_strict1_)) != "class"],
      attributes(f)
    )
    expect_identical(
      attributes(f_strict2_)[names(attributes(f_strict2_)) != "class"],
      attributes(f)
    )
    expect_identical(
      class(f_strict1_)[class(f_strict1_) != "strict_closure"],
      class(f)
    )
    expect_identical(
      class(f_strict2_)[class(f_strict2_) != "strict_closure"],
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

  f_strict_ <- strictly_(f, chk1, chk2)
  calls_ <- dplyr::arrange_(strict_checks(f_strict_), ~string)
  # 4 checks: One global check on 3 arguments, plus a check on 1 argument
  expect_identical(nrow(calls), 4L)
  expect_identical(nrow(calls_), 4L)

  f_strict2 <- strictly(f, chk1, .checklist = list(chk2))
  expect_identical(sort_checks(f_strict2), calls)
  f_strict2_ <- strictly_(f, chk1, .checklist = list(chk2))
  expect_identical(sort_checks(f_strict2_), calls_)

  f_strict3 <- strictly(f, .checklist = list(chk1, chk2))
  expect_identical(sort_checks(f_strict3), calls)
  f_strict3_ <- strictly_(f, .checklist = list(chk1, chk2))
  expect_identical(sort_checks(f_strict3_), calls_)
})

test_that("existing checks are preserved when adding new checks", {
  f0 <- function(x, y, ...) NULL
  chks <- list(
    "Not numeric" ~ is.numeric,
    list("y not nonzero" ~ y) ~ {. != 0}
  )
  f <- strictly(f0, .checklist = chks)
  chks_f <- strict_checks(f)
  f_ <- strictly_(f0, .checklist = chks)
  chks_f_ <- strict_checks(f_)

  new_chks <- list(
    "Not less than one" = list("Not less than one" ~ x) ~ {. < 1},
    "Not positive" = "Not positive" ~ {. > 0}
  )

  for (err_msg in names(new_chks)) {
    chk <- new_chks[[err_msg]]

    g <- strictly(f, chk)
    chks_g <- strict_checks(g)
    g_ <- strictly_(f_, chk)
    chks_g_ <- strict_checks(g_)

    # Checks of f subset of checks of g
    chks_f_g <- dplyr::distinct(dplyr::bind_rows(chks_g, chks_f))
    expect_identical(
      chks_f_g %>% dplyr::arrange_("string"),
      chks_g %>% dplyr::arrange_("string")
    )
    chks_f_g_ <- dplyr::distinct(dplyr::bind_rows(chks_g_, chks_f_))
    expect_identical(
      chks_f_g_ %>% dplyr::arrange_("string"),
      chks_g_ %>% dplyr::arrange_("string")
    )

    # All previous checks checked
    expect_error(g("1", 1), "Not numeric: `x`")
    expect_error(g(1, "1"), "Not numeric: `y`")
    expect_error(g(1, 0), "y not nonzero")
    expect_error(g_("1", 1), "Not numeric: `x`")
    expect_error(g_(1, "1"), "Not numeric: `y`")
    expect_error(g_(1, 0), "y not nonzero")

    # New check checked
    expect_error(g(2, -1), err_msg)
    expect_error(g_(2, -1), err_msg)
  }
})

test_that(".warn_missing = TRUE adds check for args without default value", {
  f <- function(x, y, z = 0) NULL
  f_warn  <- strictly(f, .warn_missing = TRUE)
  f_check <- strictly(f_warn, list("z not numeric" ~ z) ~ is.numeric)
  f_warn_  <- strictly_(f, .warn_missing = TRUE)
  f_check_ <- strictly_(f_warn_, list("z not numeric" ~ z) ~ is.numeric)

  # Check applied
  expect_error(f_check(1, 1, "1"), "z not numeric")
  expect_error(f_check_(1, 1, "1"), "z not numeric")

  # No warning if all required arguments supplied
  expect_warning(f_warn(1, 1), NA)
  expect_warning(f_check(1, 1), NA)
  expect_warning(f_warn_(1, 1), NA)
  expect_warning(f_check_(1, 1), NA)

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
    expect_warning(out_ <- do.call(f_warn_, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out_, do.call(f, arg))

    # Warning behavior persists in presence of checks
    expect_warning(out <- do.call(f_check, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out, do.call(f, arg))
    expect_warning(out_ <- do.call(f_check_, arg),
                   paste("Missing required argument\\(s\\):", nms))
    expect_identical(out_, do.call(f, arg))
  }
})

test_that(".warn_missing = FALSE removes missing argument check", {
  f <- function(x, y, z = 1, ...) NULL

  # .warn_missing = FALSE no effect if no missing argument checks existant
  expect_warning(f(1), NA)
  expect_identical(strictly(f, .warn_missing = FALSE), f)
  expect_warning(strictly(f, .warn_missing = FALSE)(1), NA)
  expect_identical(strictly_(f, .warn_missing = FALSE), f)
  expect_warning(strictly_(f, .warn_missing = FALSE)(1), NA)

  f_strict <- strictly(f, .warn_missing = TRUE)
  f_strict_ <- strictly_(f, .warn_missing = TRUE)

  # .warn_missing = FALSE removes missing argument check if existing
  expect_warning(f_strict(1), "Missing required argument\\(s\\): y")
  expect_identical(strictly(f_strict, .warn_missing = FALSE), f)
  expect_warning(strictly(f_strict, .warn_missing = FALSE)(1), NA)
  expect_warning(f_strict_(1), "Missing required argument\\(s\\): y")
  expect_identical(strictly_(f_strict_, .warn_missing = FALSE), f)
  expect_warning(strictly_(f_strict_, .warn_missing = FALSE)(1), NA)
})

test_that(".warn_missing = NULL preserves missing-argument-check behavior", {
  f <- function(x, y, z = 1, ...) NULL
  fns <- list(
    f,
    strictly(f, ~is.numeric),
    strictly(f, ~is.numeric, .warn_missing = TRUE)
  )
  fns_ <- list(
    f,
    strictly_(f, ~is.numeric),
    strictly_(f, ~is.numeric, .warn_missing = TRUE)
  )

  for (g in fns) {
    expect_identical(strictly(g, .warn_missing = NULL), g)
  }
  for (g_ in fns_) {
    expect_identical(strictly_(g_, .warn_missing = NULL), g_)
  }
})

test_that(".warn_missing is ignored when every named arg has default value", {
  f <- function(x = 1, ...) NULL
  f_strict <- strictly(f, ~is.numeric)
  f_strict_ <- strictly_(f, ~is.numeric)

  expect_identical(strictly(f, .warn_missing = TRUE), f)
  expect_identical(strictly(f, .warn_missing = FALSE), f)
  expect_identical(strictly(f, .warn_missing = NULL), f)
  expect_identical(strictly(f_strict, .warn_missing = TRUE), f_strict)
  expect_identical(strictly(f_strict, .warn_missing = FALSE), f_strict)
  expect_identical(strictly(f_strict, .warn_missing = NULL), f_strict)
  expect_identical(strictly_(f, .warn_missing = TRUE), f)
  expect_identical(strictly_(f, .warn_missing = FALSE), f)
  expect_identical(strictly_(f, .warn_missing = NULL), f)
  expect_identical(strictly_(f_strict_, .warn_missing = TRUE), f_strict_)
  expect_identical(strictly_(f_strict_, .warn_missing = FALSE), f_strict_)
  expect_identical(strictly_(f_strict_, .warn_missing = NULL), f_strict_)
})

test_that(".warn_missing value does not change checks", {
  f0 <- function(x, y, z = 1, ...) x + y + z
  f1 <- strictly(f0,
                 list("x not numeric" ~ x, "y not numeric" ~ y) ~ is.numeric)
  f1_ <- strictly_(f0,
                   list("x not numeric" ~ x, "y not numeric" ~ y) ~ is.numeric)

  # .warn_missing = FALSE doesn't change checks
  f1_warn_false <- strictly(f1, .warn_missing = FALSE)
  f1_warn_false_ <- strictly_(f1_, .warn_missing = FALSE)

  set.seed(1)
  args <- lapply(1:10, function(.) runif(2))
  for (arg in args) {
    out <- f0(arg[[1L]], arg[[2L]])
    # Function evaluated normally if checks pass
    expect_equal(f1(arg[[1L]], arg[[2L]]), out)
    expect_equal(f1_warn_false(arg[[1L]], arg[[2L]]), out)

    out_ <- f0(arg[[1L]], arg[[2L]])
    # Function evaluated normally if checks pass
    expect_equal(f1_(arg[[1L]], arg[[2L]]), out_)
    expect_equal(f1_warn_false_(arg[[1L]], arg[[2L]]), out_)
  }

  nonnumeric <- list(NULL, "A", log, ls, mtcars, quote(ls), TRUE, FALSE, NA)
  for (val in nonnumeric) {
    # Checks performed
    expect_error(f1(x = val, y = 1), "x not numeric")
    expect_error(f1_warn_false(x = val, y = 1), "x not numeric")
    expect_error(f1(y = val, x = 1), "y not numeric")
    expect_error(f1_warn_false(y = val, x = 1), "y not numeric")
    expect_error(f1_(x = val, y = 1), "x not numeric")
    expect_error(f1_warn_false_(x = val, y = 1), "x not numeric")
    expect_error(f1_(y = val, x = 1), "y not numeric")
    expect_error(f1_warn_false_(y = val, x = 1), "y not numeric")
  }

  # .warn_missing = TRUE doesn't change checks
  f1_warn_true <- strictly(f1, .warn_missing = TRUE)
  f1_warn_true_ <- strictly_(f1_, .warn_missing = TRUE)

  set.seed(1)
  args <- lapply(1:10, function(.) runif(2))
  for (arg in args) {
    out <- f0(arg[[1L]], arg[[2L]])

    expect_equal(f1_warn_true(arg[[1L]], arg[[2L]]), out)
    expect_equal(f1_warn_true_(arg[[1L]], arg[[2L]]), out)
  }

  for (val in nonnumeric) {
    # Checks performed
    expect_error(f1_warn_true(x = val, y = 1), "x not numeric")
    expect_error(f1_warn_true(y = val, x = 1), "y not numeric")
    expect_error(f1_warn_true_(x = val, y = 1), "x not numeric")
    expect_error(f1_warn_true_(y = val, x = 1), "y not numeric")
  }

  # Check for missing arguments performed
  expect_warning(purrr::safely(f1_warn_true)(x = 1),
                 "Missing required argument\\(s\\): y")
  expect_warning(purrr::safely(f1_warn_true)(y = 1),
                 "Missing required argument\\(s\\): x")
  expect_warning(purrr::safely(f1_warn_true_)(x = 1),
                 "Missing required argument\\(s\\): y")
  expect_warning(purrr::safely(f1_warn_true_)(y = 1),
                 "Missing required argument\\(s\\): x")
})
