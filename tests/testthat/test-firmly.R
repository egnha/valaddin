context("Firmly")

fs <- lapply(args_list, pass_args)
has_args <- map_lgl(args_list, ~ length(nomen(.)$nm) > 0L)
fs_with_args <- fs[has_args]

sort_checks_df <- function(x) x[order(x[["string"]]), , drop = FALSE]
sort_checks <- function(f) sort_checks_df(firm_checks(f))

test_that("error raised when .f is not a closure", {
  errmsg <- "`.f` not an interpreted function"
  bad_fns <- list(NULL, NA, log, 1, "A", quote(ls))

  for (f in bad_fns) {
    expect_error(firmly(f), errmsg)
  }
})

test_that("error raised when .warn_missing is not a non-NA character vector", {
  errmsg <- "`.warn_missing` not a character vector"
  bad_val <- list(NA, logical(0), logical(2), 1, 0, quote(x), list("x"))

  for (f in fs) {
    for (val in bad_val) {
      expect_error(firmly(f, .warn_missing = val), errmsg)
    }
  }
})

test_that("error raised when .error_class is not a non-NA character vector", {
  errmsg <- "`.error_class` not a character vector"
  bad_val <- list(NA, logical(0), logical(2), 1, 0, quote(x), list("x"))

  for (f in fs) {
    for (val in bad_val) {
      expect_error(firmly(f, .error_class = val), errmsg)
    }
  }
})

test_that("error raised if .warn_missing not in non-empty set of arguments", {
  err_msg <- "Invalid `\\.warn_missing`: %s not argument\\(s\\) of `\\.f`"

  for (f in fs_with_args) {
    nm <- nomen(formals(f))$nm
    expect_error(firmly(f, .warn_missing = "dummy"), sprintf(err_msg, "`dummy`"))
    expect_error(firmly(f, .warn_missing = c("dummy", nm[1L])),
                 sprintf(err_msg, "`dummy`"))
    expect_error(firmly(f, .warn_missing = c("dummy1", "dummy2")),
                 sprintf(err_msg, "`dummy1`, `dummy2`"))

  }
})

test_that("error raised if .warn_missing non-empty but no named arguments", {
  err_msg <- "Invalid `\\.warn_missing`: `\\.f` has no named argument"
  fs_wo_arg <- list(function() NULL, function(...) NULL)

  for (f in fs_wo_arg) {
    expect_error(firmly(f, .warn_missing = "dummy"), err_msg)
    expect_error(firmly(f, ~ is.numeric, .warn_missing = "dummy"), err_msg)
  }
})

test_that("error raised when .checklist is not a list", {
  errmsg <- "`.checklist` not a list"
  bad_val <- list(NA, logical(0), logical(2), 1, quote(x), letters)

  for (f in fs) {
    for (val in bad_val) {
      expect_error(firmly(f, .checklist = val), errmsg)
    }
  }
})

test_that("error raised when .checklist is an invalid checklist", {
  f <- function(x, y = 1, ...) NULL

  errmsg <- "Invalid check formula\\(e\\)"

  for (chk in invalid_checks) {
    expect_error(firmly(f, chk), errmsg)
    expect_error(firmly(f, .checklist = list(chk)), errmsg)
    expect_error(firmly(f, ~ {. > 0}, .checklist = list(chk)), errmsg)
    expect_error(firmly(f, chk, .checklist = list(~ {. > 0})), errmsg)
    expect_error(firmly(f, .checklist = list(~ {. > 0}, chk)), errmsg)
  }
})

test_that("error raised when .checklist is a non-evaluable checklist", {
  f <- function(x, y = 1, ...) NULL

  for (chk in invalid_checks) {
    expect_error(firmly(f, chk))
    expect_error(firmly(f, .checklist = list(chk)))
    expect_error(firmly(f, ~{. > 0}, .checklist = list(chk)))
    expect_error(firmly(f, chk, .checklist = list(~{. > 0})))
    expect_error(firmly(f, .checklist = list(~{. > 0}, chk)))
  }
})

test_that("function is unchanged when no named arguments & no .warn_missing", {
  fs_wo_arg <- list(function() NULL, function(...) NULL)

  for (f in fs_wo_arg) {
    expect_identical(firmly(f), f)
    expect_identical(suppressWarnings(firmly(f, ~ is.numeric)), f)
  }
})

test_that("warning raised when no named args and only check formula(e) given", {
  warn_msg <- "Check formula\\(e\\) not applied: `\\.f` has no named argument"
  fs_wo_arg <- list(function() NULL, function(...) NULL)

  for (f in fs_wo_arg) {
    expect_warning(firmly(f, ~ is.numeric), warn_msg)
  }
})

test_that("function is unchanged when no validation specified", {
  for (args in args_list) {
    f <- pass_args(args)
    expect_identical(firmly(f), f)
  }
})

test_that("function unchanged when no validation & .error_class inapplicable", {
  # When no validation (checks or .warn_missing) is given, and .f has no checks,
  # then .error_class is inapplicable.

  for (args in args_list) {
    f <- pass_args(args)

    expect_null(firm_checks(f))
    expect_identical(firmly(f, .error_class = "customError"), f)

    named_args <- setdiff(names(args), "...")
    if (length(named_args)) {
      f_warn <- firmly(f, .warn_missing = named_args[[1L]])

      expect_true(is_firm(f_warn))
      expect_null(firm_checks(f_warn))
      expect_identical(firmly(f_warn, .error_class = "customError"), f_warn)
    }
  }
})

test_that("no warning raised when no validation specified", {
  for (args in args_list) {
    f <-  pass_args(args)
    expect_warning(firmly(f), NA)
  }
})

test_that("argument signature is preserved", {
  for (args in args_list) {
    f <- new_fn(args)

    sig <- formals(f)
    expect_identical(sig, as.pairlist(args))
    expect_identical(formals(firmly(f)), sig)

    # If there are named arguments, check all possible combination or arguments
    if (length(setdiff(names(args), "..."))) {
      nm <- names(args)[names(args) != "..."][[1L]]
      sig %>%
        expect_identical(
          formals(firmly(f, ~is.numeric))
        ) %>%
        expect_identical(
          formals(firmly(f, .warn_missing = nm))
        ) %>%
        expect_identical(
          formals(firmly(f, .error_class = "customError"))
        ) %>%
        expect_identical(
          formals(firmly(f, ~is.numeric, .warn_missing = nm))
        ) %>%
        expect_identical(
          formals(firmly(f, ~is.numeric, .error_class = "customError"))
        ) %>%
        expect_identical(
          formals(firmly(f, .warn_missing = nm, .error_class = "customError"))
        ) %>%
        expect_identical(
          formals(firmly(f, ~is.numeric, .warn_missing = nm,
                         .error_class = "customError"))
        )
    }
  }
})

test_that("original function body/environment/attributes are preserved", {
  set.seed(1)

  args_list <- args_list[has_args]

  len <- sample(length(args_list))

  for (i in seq_along(args_list)) {
    junk <- paste(sample(letters, len[[i]], replace = TRUE), collapse = "")
    body <- substitute(quote(x), list(x = junk))
    attr <- setNames(as.list(sample(LETTERS)), letters)
    env <- new.env(parent = baseenv())
    f <- do.call("structure",
      c(.Data = new_fn(args_list[[i]], body, env), attr)
    )
    f_firm1 <- suppressWarnings(firmly(f, list(~x) ~ is.numeric))
    f_firm2 <- suppressWarnings(firmly(f_firm1, .warn_missing = "x"))

    # Same body
    nms <- nomen(args_list[[i]])$nm
    args <- setNames(as.list(rep(0, length(nms))), nms)
    expect_identical(do.call(f_firm1, args), junk)
    expect_identical(do.call(f_firm2, args), junk)

    # Same environment
    if (is.null(firm_core(f_firm1))) {
      expect_identical(environment(f_firm1), environment(f))
      expect_identical(environment(f_firm2), environment(f))
    } else {
      expect_identical(environment(firm_core(f_firm1)), environment(f))
      expect_identical(environment(firm_core(f_firm2)), environment(f))
    }

    # Same attributes
    expect_identical(
      attributes(f_firm1)[names(attributes(f_firm1)) != "class"],
      attributes(f)
    )
    expect_identical(
      attributes(f_firm2)[names(attributes(f_firm2)) != "class"],
      attributes(f)
    )
    expect_identical(
      class(f_firm1)[class(f_firm1) != "firm_closure"],
      class(f)
    )
    expect_identical(
      class(f_firm2)[class(f_firm2) != "firm_closure"],
      class(f)
    )
  }
})

test_that("checks in ... are combined with .checklist", {
  f <- function(x, y = x, z = 0, ...) NULL
  chk1 <- list(~x) ~ {. > 0}
  chk2 <- ~is.numeric

  f_firm <- firmly(f, chk1, chk2)
  calls <- sort_checks(f_firm)

  # 4 checks: One global check on 3 arguments, plus a check on 1 argument
  expect_identical(nrow(calls), 4L)

  f_firm2 <- firmly(f, chk1, .checklist = list(chk2))
  expect_identical(sort_checks(f_firm2), calls)

  f_firm3 <- firmly(f, .checklist = list(chk1, chk2))
  expect_identical(sort_checks(f_firm3), calls)
})

test_that("existing checks are preserved when adding new checks", {
  f0 <- function(x, y, ...) NULL
  chks <- list(
    "Not numeric" ~ is.numeric,
    list("y not nonzero" ~ y) ~ {. != 0}
  )
  f <- firmly(f0, .checklist = chks)
  chks_f <- firm_checks(f)

  new_chks <- list(
    "Not less than one" = list("Not less than one" ~ x) ~ {. < 1},
    "Not positive" = "Not positive" ~ {. > 0}
  )

  for (err_msg in names(new_chks)) {
    chk <- new_chks[[err_msg]]

    g <- firmly(f, chk)
    chks_g <- firm_checks(g)

    # Checks of f subset of checks of g
    chks_f_g <- unique(rbind(chks_g, chks_f))
    expect_identical(sort_checks_df(chks_f_g), sort_checks_df(chks_g))

    # All previous checks checked
    expect_error(g("1", 1), "Not numeric: `x`")
    expect_error(g(1, "1"), "Not numeric: `y`")
    expect_error(g(1, 0), "y not nonzero")

    # New check checked
    expect_error(g(2, -1), err_msg)
  }
})

test_that(".warn_missing adds missing-argument check", {
  warn_msg <- "Argument\\(s\\) expected but not specified in call"
  f <- function(x = 0, y = 0, ...) NULL

  seq_arg <- list("x", "y", c("x", "y"))
  for (args in seq_arg) {
    f_warn <- firmly(f, .warn_missing = args)
    f_warn_firm <- firmly(f_warn, ~ is.numeric)

    expect_warning(f_warn(u = "part of dots"), warn_msg)
    expect_warning(f_warn(u = "part of dots"), quote_collapse(args))
    expect_warning(f_warn_firm(u = "part of dots"), warn_msg)
    expect_warning(f_warn_firm(u = "part of dots"), quote_collapse(args))
  }
})

test_that(".warn_missing value does not change checks", {
  warn_msg <- "Argument\\(s\\) expected but not specified in call"

  f <- function(x, y, z = 1, ...) x + y + z
  ff <- firmly(
    f,
    list("x not numeric" ~ x, "y not numeric" ~ y) ~ is.numeric,
    .warn_missing = "z"
  )

  # .warn_missing
  args <- {set.seed(1); lapply(1:10, function(.) runif(2))}
  for (arg in args) {
    out <- f(arg[[1L]], arg[[2L]])
    expect_equal(suppressWarnings(ff(arg[[1L]], arg[[2L]])), out)
  }

  # Checks still performed
  nonnumeric <- list(NULL, "A", log, ls, mtcars, quote(ls), TRUE, FALSE, NA)
  for (val in nonnumeric) {
    expect_error(suppressWarnings(ff(x = val, y = 1)), "x not numeric")
    expect_error(suppressWarnings(ff(y = val, x = 1)), "y not numeric")
  }
  # Check for missing arguments performed
  expect_warning(ff(x = 0, y = 1), warn_msg)
  expect_warning(ff(x = 0, y = 1), "`z`")
})

test_that("arguments, whether checked or not, are evaluated lazily", {
  f <- function(x, y) if (x) "True" else y
  msg <- "`y` not an error"
  chk_is_error <-
    list(msg ~ tryCatch({y; FALSE}, error = function(e) TRUE)) ~ isTRUE

  f_firm <- firmly(f, chk_is_error)

  # Verify that y is checked:
  # - Pass
  expect_error(f_firm(TRUE, stop("!")), NA)
  # - Fail
  not_error <- list(TRUE, FALSE, NULL, NA_real_, NaN, letters, mtcars)
  for (. in not_error) expect_error(f_firm(TRUE, .), msg)

  # Verify that y is not forced when function called
  expect_identical(f_firm(TRUE, stop("!")), "True")
})
