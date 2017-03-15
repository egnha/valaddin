context("Input validation")

test_that("function return value exactly reproduced when all checks pass", {
  fs <- lapply(args_list, pass_args)

  chklist <- list(~is.numeric, ~{. > 0}, "Not TRUE when coerced" ~ as.logical)

  for (f in fs) {
    l <- length(formals(f))
    # Arguments as list of positive numbers (if not empty)
    args <- if (l) as.list(1:l) else list()
    out <- do.call(f, args)
    f_firm <- suppressWarnings(firmly(f, .checklist = chklist))

    expect_identical(do.call(f_firm, args), out)
  }
})

test_that("'{...}' predicate expression interpreted as lambda function", {
  f <- identity

  chk <- list("FALSE" ~ x)
  predicate <- function(x) if (identical(x, TRUE)) x else FALSE

  # Only allow f to return TRUE
  chks <- list(
    named = chk ~ predicate,
    anond = chk ~ {if (identical(., TRUE)) . else FALSE}
  )
  f_firm <- lapply(chks, firmly, .f = f)

  # Pass
  Reduce(function(out, f.) expect_identical(out, f.(TRUE)), f_firm, f(TRUE))

  # Fail
  bad_input <- list(FALSE, 1, "A", log, quote({cat("Yo!"); sin(1 + pi)}))
  for (x in bad_input) {
    for (f. in f_firm) {
      expect_error(f.(x), "FALSE")
    }
  }
})

test_that("one-sided formula produces global check", {
  f <- pass_args(alist(x = , y = , z = 0, ... = , u = y - z, v = ))
  f_num <- firmly(f, ~is.numeric)
  f_pos <- firmly(f_num, ~{. > 0})

  # Pass
  out <- f(1, 2, 3, u = 4, v = 5)
  expect_identical(f_num(1, 2, 3, u = 4, v = 5), out)
  expect_identical(f_pos(1, 2, 3, u = 4, v = 5), out)

  # Check failure
  nms <- c("x", "y", "z", "u", "v")
  arg_list <- lapply(1:3, function(n) {
    as.list((1:5) * c(rep(-1, n), rep(1, 5 - n))) %>% setNames(nms)
  })
  for (i in seq_along(arg_list)) {
    for (arg in nms[1:i]) {
      expect_error(purrr::lift(f_pos)(arg_list[[i]]),
                   sprintf("FALSE[^\n]*?\\(%s\\)", arg))
    }
    # No other errors
    expect_equal(purrr::lift(purrr::safely(f_pos))(arg_list[[i]]) %>% {
      str_count(.$error, "FALSE")
    }, i)
  }

  # Error evaluating check because of missing argument
  expect_equivalent(f(1, 2), list(1, 2))
  expect_error(f_num(1, 2), "Error evaluating check.*?argument \"v\" is missing")
  expect_error(f_pos(1, 2), "Error evaluating check.*?argument \"v\" is missing")

  # Error evaluating check because of invalid input types
  args <- list(1, "y", v = 0)

  expect_error(do.call("f_pos", args), "FALSE[^\n]*?is\\.numeric\\(y\\)")
  expect_error(do.call("f_pos", args),
               "FALSE[^\n]*?function\\(.\\) \\{\\. > 0\\}\\)\\(z\\)")
  expect_error(do.call("f_pos", args),
               "FALSE[^\n]*?function\\(.\\) \\{\\. > 0\\}\\)\\(v\\)")
  expect_error(do.call("f_pos", args),
               "Error evaluating check.*?\\{\\. > 0\\}\\)\\(u\\)")

  # No other errors
  expect_n_errors(3, f_pos, args, "FALSE")
  expect_n_errors(2, f_pos, args, "Error evaluating check")
})

test_that("string formula produces global check with message", {
  f <- pass_args(alist(x = , y = , z = 0, ... = , u = y - z, v = ))
  f_num <- firmly(f, "Not numeric" ~ is.numeric)
  f_pos <- firmly(f_num, "Not positive" ~ {. > 0})

  # Pass
  out <- f(1, 2, 3, u = 4, v = 5)
  expect_identical(f_num(1, 2, 3, u = 4, v = 5), out)
  expect_identical(f_pos(1, 2, 3, u = 4, v = 5), out)

  # Check failure
  nms <- c("x", "y", "z", "u", "v")
  arg_list <- lapply(1:3, function(n) {
    as.list((1:5) * c(rep(-1, n), rep(1, 5 - n))) %>% setNames(nms)
  })
  for (i in seq_along(arg_list)) {
    for (arg in nms[1:i]) {
      expect_error(purrr::lift(f_pos)(arg_list[[i]]),
                   sprintf("Not positive: `%s`", arg))
    }
    # No other errors
    expect_n_errors(i, f_pos, arg_list[[i]], "Not positive")
  }

  # Error evaluating check because of missing argument
  expect_equivalent(f(1, 2), list(1, 2))
  expect_error(f_num(1, 2), "Error evaluating check.*?argument \"v\" is missing")
  expect_error(f_pos(1, 2), "Error evaluating check.*?argument \"v\" is missing")

  # Error evaluating check because of invalid input types
  args <- list(1, "y", v = 0)

  expect_error(do.call("f_pos", args), "Not numeric: `y`")
  expect_error(do.call("f_pos", args), "Not positive: `z`")
  expect_error(do.call("f_pos", args), "Not positive: `v`")
  expect_error(do.call("f_pos", args),
               "Error evaluating check.*?is\\.numeric\\(u\\)")
  expect_error(do.call("f_pos", args),
               "Error evaluating check.*?function\\(\\.\\) \\{\\. > 0\\}\\)\\(u\\)")

  # No other errors
  expect_n_errors(1, f_pos, args, "Not numeric")
  expect_n_errors(2, f_pos, args, "Not positive")
  expect_n_errors(2, f_pos, args, "Error evaluating check")
})

test_that("unnamed checks in checklist formula use auto-generated messages", {
  has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
  fs <- lapply(args_list[has_xy], pass_args)

  chklist <- list(list(~x, "y not numeric" ~ y) ~ is.numeric)

  non_numeric <- list(NULL, NA, "string", TRUE, sin, quote({cat("Ho!")}))
  for (f in fs) {
    f_firm <- firmly(f, .checklist = chklist)

    for (x in non_numeric) {
      args <- list(x = x, y = 0)
      expect_error(do.call(f_firm, args, quote = TRUE),
                   "FALSE[^\n]*?is\\.numeric\\(x\\)")
      expect_n_errors(1, f_firm, args, "FALSE")
      expect_n_errors(0, f_firm, args, "y not numeric")
    }
  }
})

test_that("named checks in checklist formula use custom messages", {
  has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
  fs <- lapply(args_list[has_xy], pass_args)

  errmsg <- "y not numeric"
  chklist <- list(list(~x, errmsg ~ y) ~ is.numeric)

  non_numeric <- list(NULL, NA, "string", TRUE, sin, quote({cat("Ho!")}))
  for (f in fs) {
    f_firm <- firmly(f, .checklist = chklist)

    for (y in non_numeric) {
      args <- list(x = 0, y = y)
      expect_error(do.call(f_firm, args, quote = TRUE), errmsg)
      expect_n_errors(0, f_firm, args, "FALSE")
      expect_n_errors(1, f_firm, args, errmsg)
    }
  }
})

test_that("predicate function of list-argument applies to argument lists", {
  f <- pass_args(alist(x = , y = , z = , ... = ))

  setup_base <- function(nms, args, fnc = f) {
    new_args <- setNames(args, nms)
    out <- do.call(fnc, new_args)
    list(args = new_args, out = out)
  }

  # Check: x, y, z numeric, x < y < z
  not_gt <- function(a, b) sprintf("%s not greater than %s", a, b)
  chklist <- list(
    ~is.numeric,
    list(not_gt("y", "x") ~ list(x, y), not_gt("z", "y") ~ list(y, z)) ~
      purrr::lift(function(a, b) b - a > 0)
  )
  f_firm <- firmly(f, .checklist = chklist)

  set.seed(1)
  for (i in 100) {
    args <- as.list(cumsum(runif(3, 0, 1)))

    base <- setup_base(c("x", "y", "z"), args)
    expect_identical(do.call(f_firm, base$args), base$out)

    base <- setup_base(c("x", "z", "y"), args)
    expect_error(do.call(f_firm, base$args), not_gt("z", "y"))

    base <- setup_base(c("y", "x", "z"), args)
    expect_error(do.call(f_firm, base$args), not_gt("y", "x"))

    base <- setup_base(c("z", "x", "y"), args)
    expect_error(do.call(f_firm, base$args), not_gt("z", "y"))

    base <- setup_base(c("y", "z", "x"), args)
    expect_error(do.call(f_firm, base$args), not_gt("y", "x"))

    base <- setup_base(c("z", "y", "x"), args)
    expect_error(do.call(f_firm, base$args), not_gt("y", "x"))
    expect_error(do.call(f_firm, base$args), not_gt("z", "y"))
  }
})

test_that("invalid predicate value flagged by precise error of such", {
  fncmsg <- "pass"
  errmsg <- "Not numeric"
  f <- function(x) fncmsg
  is_long_lgl <- function(x) is.logical(x) && (length(x) >= 2L)
  is_numeric_faulty <- function(x) {
    if (is.null(x) || is.na(x) || identical(x, logical(0)) || is_long_lgl(x)) {
      x
    } else {
      is.numeric(x)
    }
  }
  f_firm <- firmly(f, list(errmsg ~ x) ~ is_numeric_faulty)

  set.seed(1)

  # Pass
  for (x in runif(10, -1, 1)) expect_identical(f(x), fncmsg)

  # Fail because predicate returns FALSE
  bad_x <- list(log, identity, "string", TRUE, quote({cat("Ho!")}), TRUE)
  for (x in bad_x) {
    expect_error(f_firm(x), errmsg)
  }

  # Fail because predicate returns invalid value
  # Predicate is_numeric_faulty() leaves these values unchanged
  for (x in list(NULL, NA, logical(0), c(TRUE, TRUE), c(TRUE, NA))) {
    expect_error(f_firm(x), "not TRUE/FALSE")
  }
})

test_that("check-eval error when check-formula variable not function variable", {
  # Functions with non-empty, non-dots-only argument signature
  fs <- map_lgl(args_list, ~ length(nomen(.)$nm) != 0L) %>% {
    lapply(args_list[.], pass_args)
  }

  # a, b are not named arguments of any f() in fs
  chklist <- list(list(~x, ~y, ~z, ~a, ~x + b) ~ is.numeric)
  named_args <- c("x", "y", "z")

  for (f in fs) {
    f_firm <- firmly(f, .checklist = chklist)

    sig <- formals(f)
    l <- length(sig)
    args <- as.list(1:l)
    missing_args <- setdiff(named_args, nomen(sig)$nm)

    for (arg in missing_args) {
      expect_error(
        do.call(f_firm, args),
        sprintf("Error evaluating check.*?object '%s' not found", arg)
      )
    }
    expect_error(do.call(f_firm, args),
                 "Error evaluating check.*?object 'a' not found")
    expect_error(do.call(f_firm, args),
                 "Error evaluating check.*?object 'b' not found")
    # No other check-evaluation errors
    expect_n_errors(2L + length(missing_args),
                    f_firm, args, "Error evaluating check")
  }
})

test_that("predicate is evaluated in its ambient formula environment", {
  has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
  fs <- lapply(args_list[has_xy], pass_args)

  predicate <- function(x) identical(x, "external")
  chk <- list("Not external" ~ x) ~ predicate

  for (f in fs) {
    f_ext <- firmly(f, list("Not external" ~ x) ~ predicate)
    g <- (function() {
      parent <- parent.frame()
      predicate <- function(x) identical(x, "internal")
      list(
        int  = firmly(f, list("Not internal" ~ x) ~ predicate),
        # Evaluate in enclosure
        ext1 = eval(quote(firmly(f, list("Not external" ~ x) ~ predicate)),
                    parent),
        # Evaluate locally but evaluate check in enclosure
        ext2 = firmly(f, eval(quote(list("Not external" ~ x) ~ predicate),
                                parent)),
        # Evaluate locally but reference check in enclosure
        ext3 = firmly(f, chk)
      )
    })()

    expect_error(f_ext(x = "external"), NA)
    expect_error(g$ext1(x = "external"), NA)
    expect_error(g$ext2(x = "external"), NA)
    expect_error(g$ext3(x = "external"), NA)
    expect_error(g$int(x = "internal"), NA)

    expect_error(f_ext(x = "internal"), "Not external")
    expect_error(g$ext1(x = "internal"), "Not external")
    expect_error(g$ext2(x = "internal"), "Not external")
    expect_error(g$ext3(x = "internal"), "Not external")
    expect_error(g$int(x = "external"), "Not internal")
  }
})

test_that("names in checking procedure don't override function arguments", {
  # Names in execution environment of validating_closure()
  nms <- c("call", "parent", "encl", "env", "verdict", "pass", "fail",
           "msg_call", "msg_error", ".chks", ".sig", ".fn", ".warn")
  def_args <- setNames(seq_along(nms), nms)
  f <- eval(call("function", as.pairlist(def_args), quote("Pass")))
  f_firm <- firmly(f, "Not numeric" ~ is.numeric)

  # Check that f, f_firm are correctly defined
  expect_error(f_firm(), NA)
  expect_identical(f_firm(), f())

  # ~ 10k possible combinations of arguments, so randomly sample them instead
  subsets <- expand.grid(rep(list(c(TRUE, FALSE)), length(nms)))
  rows <- {set.seed(1); sample.int(nrow(subsets), 100L)}
  for (i in rows) {
    # Make invalid argument list (i.e., non-numeric)
    subset <- t(subsets[i, ])
    args <- as.list(setNames(nm = nms[subset]))

  expect_n_errors(n = length(args), f_firm, args, "Not numeric")
  }
})
