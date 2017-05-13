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

  expect_error(do.call("f_pos", args),
               "FALSE[^\n]*?is\\.numeric\\(y\\)")
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

test_that("warnings that arise when validating inputs are suppressed", {
  is_numeric <- function(x) {
    out <- tryCatch(is.numeric(x), warning = identity)
    if (inherits(out, "warning")) {
      message(conditionMessage(out))
      suppressWarnings(is.numeric(x))
    } else {
      out
    }
  }
  f <- function(x) NULL
  ff <- firmly(f, ~is_numeric)

  # No error or warning or messages when evaluating core function
  expect_error(f(log(-1)), NA)
  expect_warning(f(log(-1)), NA)
  expect_message(f(log(-1)), NA)

  # Still no error when validating inputs
  expect_error(ff(log(-1)), NA)

  # Warning created when validating inputs captured as a message
  expect_message(ff(log(-1)), "NaNs produced")

  # However, the warning itself is suppressed
  expect_warning(ff(log(-1)), NA)
})

test_that("default input validation error subclass is 'simpleError'", {
  f <- firmly(function(x) x, ~is.numeric)
  error_class <- class(tryCatch(f("0"), error = identity))
  expect_identical(error_class, c("simpleError", "error", "condition"))
})

test_that("input validation error is signaled by error subclass .error_class", {
  error_classes <- list(
    character(),
    "specialError",
    c("extraSpecialError", "specialError")
  )

  for (subclass in error_classes) {
    f <- firmly(function(x) x, ~is.numeric, .error_class = subclass)

    expect_identical(
      class(tryCatch(f("0"), error = identity)),
      c(subclass %||% "simpleError", "error", "condition")
    )
  }
})

test_that("error subclass changes only when .error_class is given", {
  get_error_class <- function(expr) class(tryCatch(expr, error = identity))
  errorfy <- function(x) c(x, "error", "condition")

  f <- firmly(function(x) x, ~is.numeric)
  g <- firmly(f, ~{. > 0})
  h <- firmly(g, .error_class = "newError")
  i <- firmly(h, .warn_missing = "x")
  j <- firmly(i, ~{log(.) > 1}, .error_class = "newerError")

  expect_identical(get_error_class(f("0")), errorfy("simpleError"))
  expect_identical(get_error_class(g("0")), errorfy("simpleError"))
  expect_identical(get_error_class(h("0")), errorfy("newError"))
  expect_identical(get_error_class(i("0")), errorfy("newError"))
  expect_identical(get_error_class(j("0")), errorfy("newerError"))
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

test_that("formal arguments don't override names in validation procedure", {
  # Bindings in execution/enclosing environment of validating_closure()
  nms    <- c("call", "encl", "env", "verdict", "pass", "fail",
              "msg_call", "msg_error", ".chks", ".sig", "exprs")
  nms_fn <- c(".fn", ".warn", "deparse_w_defval", "enumerate_many", "error",
              "problems", "promises")

  sum_args <- parse(text = paste(nms, collapse = "+"))
  f <- function() {
    # Ensure that no arguments have are logical (thus are coercible to numeric)
    args <- as.list(match.call()[-1])
    stopifnot(vapply(args, Negate(is.logical), logical(1)))
    eval(sum_args)
  }

  # All function arguments get the function wrong_fn as default value
  wrong_fn <- function(...) {
    message("This function shouldn't have been called!")
  }
  def_args_fn <- stats::setNames(vector("list", length(nms_fn)), nms_fn)
  def_args_fn[] <- list(quote(wrong_fn))

  # All non-function arguments get an positive integer default value
  def_args_nonfn <- stats::setNames(seq_along(nms), nms)
  def_args <- c(def_args_fn, def_args_nonfn)
  formals(f) <- as.pairlist(def_args)

  make_checkfml <- function(.) eval(parse(text = paste0("~", .)))
  f_firm <- firmly(f,
                   lapply(nms_fn, make_checkfml) ~ is.function,
                   lapply(nms, make_checkfml) ~ is.numeric)
  f_warn <- firmly(f, .warn_missing = names(def_args))

  # Verify that f_firm(), f_warn() generate no errors and return correct value
  expect_error(f_firm(), NA)
  expect_error(do.call("f_warn", def_args), NA)
  sum(def_args_nonfn) %>%
    expect_identical(f_firm()) %>%
    expect_identical(do.call("f_warn", def_args))

  # Verify that f_firm(), f_warn() do not call its function arguments
  msg <- capture_messages(wrong_fn())
  expect_gt(length(msg), 0)        # msg is non-empty string
  expect_message(wrong_fn(), msg)  # wrong_fn() produces msg when called
  # f_firm(), f_warn() produce no message, so wrong_fn() wasn't called
  expect_message(f_firm(), NA)
  expect_message(do.call("f_warn", def_args), NA)
})

context("Input-validation environment")

test_that("default values of arguments can be validated", {
  foo <- local({
    internal <- "internal"
    function(y, x = internal) NULL
  })
  parent.env(environment(foo)) <- baseenv()

  foo_firm_chr <- (function(f) firmly(f, list(~x) ~ is.character))(foo)
  foo_firm_num <- (function(f) firmly(f, list(~x) ~ is.numeric))(foo)

  expect_error(foo_firm_chr(), NA)
  expect_error(foo_firm_num(), "FALSE: is.numeric\\(x\\)")
})

test_that("checking a missing argument raises an error", {
  foo <- firmly(function(x) NULL, ~is.character)

  expect_error(foo("x"), NA)
  expect_error(foo(), "Error evaluating check.*?argument \"x\" is missing")
})

test_that("promise names don't collide with names in predicate environment", {
  foo <- local({
    a <- 1
    function(x, y = a) list(x, y)
  })
  parent.env(environment(foo)) <- baseenv()

  foo_firm <- local({
    x <- "x in predicate environment"
    y <- "y in predicate environment"
    a <- "a in predicate environment"
    predicate <- function(x) is.numeric(x)
    firmly(foo, ~predicate)
  })

  # Verify that 'x' is a promise, not the 'x' in environment(foo_firm)
  expect_error(foo(0), NA)
  expect_error(foo_firm(environment(foo_firm)$x), "FALSE: predicate\\(x\\)")

  # Verify that 'y' is a promise, not the 'y' in environment(foo_firm)
  expect_error(foo(0, 1), NA)
  expect_error(foo_firm(0, environment(foo_firm)$y), "FALSE: predicate\\(y\\)")

  # Objects in environment(foo_firm) won't substitute missing promises
  expect_error(foo_firm(), "Error evaluating check.*?argument \"x\" is missing")
})

test_that("promise with same name as predicate doesn't hijack predicate", {
  p <- function(x) is.function(x)
  foo <- function(p) NULL
  foo_firm <- firmly(foo, list("Argument 'p' is not a function" ~ p) ~ p)

  # 'p' as an argument is a promise
  expect_error(foo_firm(p), NA)
  expect_error(foo_firm(1), "Argument 'p' is not a function")

  # Verify that the predicate function 'p' is not the argument 'p'
  expect_error(foo_firm(function(...) FALSE), NA)
})

test_that("non-promise objects in validation expression come from predicate", {
  a <- ""
  foo <- function(x) x

  foo_firm <- local({
    a <- "Non-empty string"
    firmly(foo, list(~paste0(x, a)) ~ {identical(., "Non-empty string")})
  })

  # 'a' is empty
  expect_false(nzchar(a))

  # But non-empty 'a' from predicate is used in validation expression
  expect_error(foo_firm(""), NA)
})
