context("Input validation")

test_that("anonymous predicate function is correctly interpreted", {
  f <- identity

  chk <- list("FALSE" ~ x)
  predicate <- function(x) if (identical(x, TRUE)) x else FALSE

  # Only allow f to return TRUE
  f_strict <- list(
    named = chk ~ predicate,
    anond = chk ~ {if (identical(., TRUE)) . else FALSE},
    anonx = chk ~ {if (identical(.x, TRUE)) .x else FALSE}
  ) %>%
    lapply(strictly, .f = f)

  # Pass
  Reduce(function(out, f.) expect_identical(out, f.(TRUE)), f_strict, f(TRUE))

  bad_input <- list(FALSE, 1, "A", log, quote({cat("Yo!"); sin(1 + pi)}))
  for (x in bad_input) {
    for (f. in f_strict) {
      expect_error(f.(x), "FALSE")
    }
  }
})

test_that("one-sided formula produces global check", {
  f <- pass_args(alist(x = , y = , z = 0, ... = , u = y - z, v = ))
  f_num <- strictly(f, ~is.numeric)
  f_pos <- strictly(f_num, ~{. > 0})

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
    expect_error(f_pos(1, "y", v = 0), "FALSE[^\n]*?is\\.numeric\\(y\\)")
    expect_error(f_pos(1, "y", v = 0),
                 "FALSE[^\n]*?\\(\\~\\{\\. > 0\\}\\)\\)\\(z\\)")
    expect_error(f_pos(1, "y", v = 0),
                 "FALSE[^\n]*?\\(\\~\\{\\. > 0\\}\\)\\)\\(v\\)")
    expect_error(f_pos(1, "y", v = 0),
                 "Error evaluating check.*?is\\.numeric\\(u\\)")
    expect_error(f_pos(1, "y", v = 0),
                 "Error evaluating check.*?\\(\\~\\{\\. > 0\\}\\)\\)\\(u\\)")

  # No other errors
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "FALSE")
  }, 3)
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Error evaluating check")
  }, 2)
})

test_that("string formula produces global check with message", {
  f <- pass_args(alist(x = , y = , z = 0, ... = , u = y - z, v = ))
  f_num <- strictly(f, "Not numeric" ~ is.numeric)
  f_pos <- strictly(f_num, "Not positive" ~ {. > 0})

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
    expect_equal(purrr::lift(purrr::safely(f_pos))(arg_list[[i]]) %>% {
      str_count(.$error, "Not positive")
    }, i)
  }

  # Error evaluating check because of missing argument
  expect_equivalent(f(1, 2), list(1, 2))
  expect_error(f_num(1, 2), "Error evaluating check.*?argument \"v\" is missing")
  expect_error(f_pos(1, 2), "Error evaluating check.*?argument \"v\" is missing")

  # Error evaluating check because of invalid input types
  expect_error(f_pos(1, "y", v = 0), "Not numeric: `y`")
  expect_error(f_pos(1, "y", v = 0), "Not positive: `z`")
  expect_error(f_pos(1, "y", v = 0), "Not positive: `v`")
  expect_error(f_pos(1, "y", v = 0),
               "Error evaluating check.*?is\\.numeric\\(u\\)")
  expect_error(f_pos(1, "y", v = 0),
               "Error evaluating check.*?\\(\\~\\{\\. > 0\\}\\)\\)\\(u\\)")
  # No other errors
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Not numeric")
  }, 1)
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Not positive")
  }, 2)
  expect_equal(purrr::safely(f_pos)(1, "y", v = 0) %>% {
    str_count(.$error, "Error evaluating check")
  }, 2)
})

test_that("function value is reproduced when all checks pass", {
  fs <- lapply(args_list, pass_args)

  chklist <- list(~is.numeric, ~{. > 0}, "Not TRUE when coerced" ~ as.logical)

  for (f in fs) {
    l <- length(formals(f))
    # Arguments as list of positive numbers (if not empty)
    args <- if (l) as.list(1:l) else list()
    out <- do.call(f, args)
    f_strict <- strictly(f, .checklist = chklist)

    expect_identical(do.call(f_strict, args), out)
  }
})

test_that("unnamed checks in checklist formula use auto-generated messages", {})

test_that("named checks in checklist formula use custom messages", {})

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
  f_strict <- strictly(f, .checklist = chklist)

  set.seed(1)
  for (i in 100) {
    args <- as.list(cumsum(runif(3, 0, 1)))

    base <- setup_base(c("x", "y", "z"), args)
    expect_identical(do.call(f_strict, base$args), base$out)

    base <- setup_base(c("x", "z", "y"), args)
    expect_error(do.call(f_strict, base$args), not_gt("z", "y"))

    base <- setup_base(c("y", "x", "z"), args)
    expect_error(do.call(f_strict, base$args), not_gt("y", "x"))

    base <- setup_base(c("z", "x", "y"), args)
    expect_error(do.call(f_strict, base$args), not_gt("z", "y"))

    base <- setup_base(c("y", "z", "x"), args)
    expect_error(do.call(f_strict, base$args), not_gt("y", "x"))

    base <- setup_base(c("z", "y", "x"), args)
    expect_error(do.call(f_strict, base$args), not_gt("y", "x"))
    expect_error(do.call(f_strict, base$args), not_gt("z", "y"))
  }
})

test_that("invalid predicate value flagged by a precise error of such", {
  fncmsg <- "pass"
  errmsg <- "Not numeric"
  f <- function(x) fncmsg
  is_long_lgl <- function(x) is.logical(x) && (length(x) >= 2L)
  is_numeric_faulty <- function(x) {
    if (is.null(x) || is.na(x) || identical(x, logical(0)) || is_long_lgl(x))
      x
    else
      is.numeric(x)
  }
  f_strict <- strictly(f, list(errmsg ~ x) ~ is_numeric_faulty)

  set.seed(1)

  # Pass
  for (x in runif(10, -1, 1)) expect_identical(f(x), fncmsg)

  # Fail because predicate returns FALSE
  bad_x <- list(log, identity, "string", TRUE, quote({cat("Ho!")}), TRUE)

  for (x in bad_x) expect_error(f_strict(x), errmsg)

  # Fail because predicate returns invalid value
  types <- c("NULL", "NA", "logical void", rep("not logical scalar", 2))
  # Predicate is_numeric_faulty() leaves these values unchanged
  bad_x <- list(NULL, NA, logical(0), c(TRUE, TRUE), c(TRUE, NA)) %>%
    setNames(type)

  for (i in seq_along(bad_x)) {
    x <- bad_x[[i]]
    type <- types[[i]]
    expect_error(f_strict(x), sprintf("Predicate value is %s", type))
  }
})