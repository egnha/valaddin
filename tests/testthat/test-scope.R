context("Scope conversion")

expect_error_p <- perlize(expect_error)

is_positive <- function(x) x > 0

chks_gbl <- list(
  "Not numeric" ~ is.numeric,
  "Not positive" ~ is_positive,
  "Not positive" ~ function(x) x > 0,
  "Not positive" ~ {. > 0}
)

chks_nongbl <- list(
  log,
  NULL,
  NA,
  letters,
  ~ is.numeric,
  list(~ x) ~ {. > 0},
  list("Not numeric" ~ x, ~ y) ~ is.numeric
)

test_that("local checker creates checks for supplied arguments", {
  err_fail <- "Not positive"
  # Test all forms of predicate function (named, anonymous, lambda)
  chks <- chks_gbl[map_lgl(chks_gbl, ~ lazyeval::f_lhs(.) == err_fail)]
  chkrs <- lapply(chks, localize)

  has_xy <- map_lgl(args_list, ~ all(c("x", "y") %in% names(.)))
  fs <- lapply(args_list[has_xy], pass_args)

  for (f in fs)
    for (chkr in chkrs) {
      # Passing checks
      expect_error(firmly(f, chkr(~ x))(x = 1, y = "y"), NA)
      expect_error(firmly(f, chkr(~ x, ~ y))(x = 1, y = 2), NA)
      expect_error(firmly(f, chkr(~ y - x))(x = 1, y = 2), NA)

      # Failing checks
      expect_n_errors(1, firmly(f, chkr(~ x)), list(x = -1, y = "y"), err_fail)
      expect_n_errors(1, firmly(f, chkr(~ x, ~ y)), list(x = -1, y = 2), err_fail)
      expect_n_errors(2, firmly(f, chkr(~ x, ~ y)), list(x = -1, y = -2), err_fail)
      expect_n_errors(1, firmly(f, chkr(~ y - x)), list(x = 2, y = 1), err_fail)

      # Predicate-evaluation failure
      err_eval <- "Error evaluating check"
      expect_error(firmly(f, chkr(~ x))(x = log("a"), y = "y"), err_eval)
      expect_error(firmly(f, chkr(~ x, ~ y))(x = -1, y = log("a")), err_eval)
      expect_error(firmly(f, chkr(~ x, ~ y))(x = log(), y = log("b")), err_eval)
      expect_error(firmly(f, chkr(~ y - x))(x = 2, y = log("b")), err_eval)

      # Invalid predicate value
      err_pred <- "not TRUE/FALSE"
      expect_error(firmly(f, chkr(~ x))(x = integer(), y = "y"), err_pred)
      expect_error(firmly(f, chkr(~ x, ~ y))(x = -1, y = integer()), err_pred)
      expect_error(firmly(f, chkr(~ x, ~ y))(x = NA, y = integer()), err_pred)
      expect_error(firmly(f, chkr(~ y - x))(x = 2, y = NA), err_pred)
    }
})

test_that("local checker uses custom error message, when provided", {
  f <- function(x, y) "Pass"

  for (chk in chks_gbl[-1L]) {
    chkr <- localize(chk)
    ff <- firmly(f, chkr(~x, "y not bigger than x" ~ y - x))

    expect_error(ff(0, 1), "Not positive: x")
    expect_error(ff(0, 0), "Not positive: x")
    expect_error(ff(0, 0), "y not bigger than x")
    expect_error(ff(2, 1), "y not bigger than x")
  }
})

test_that("local checker evaluates predicate in global formula environment", {
  predicate <- function(x) identical(x, "external")
  chk <- "Not external" ~ predicate

  f <- function(x) NULL
  f_ext <- firmly(f, localize(chk)(~ x))
  g <- (function() {
    parent <- parent.frame()
    predicate <- function(x) identical(x, "internal")
    list(
      int  = firmly(f, localize("Not internal" ~ predicate)(~ x)),
      # Evaluate in enclosure
      ext1 = eval(
        quote(firmly(f, localize("Not external" ~ predicate)(~ x))),
        parent
      ),
      # Evaluate locally but evaluate check in enclosure
      ext2 = firmly(f, eval(
        quote(localize("Not external"  ~ predicate)), parent)(~ x)
        ),
      # Evaluate locally but reference check in enclosure
    ext3 = firmly(f, localize(chk)(~ x))
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
})

test_that("local checker raises error if arg not onesided/LHS-string formula", {
  not_onesided_lhs_string <- list(
    "string",
    1,
    NULL,
    list(),
    logical(0),
    NA,
    list(~ x),
    list("string" ~ x),
    quote(x)
  )

  for (chk in chks_gbl) {
    chkr <- localize(chk)
    expect_error(chkr(xxx), "object 'xxx' not found")
    for (x in not_onesided_lhs_string)
      expect_error(chkr(x), "LHS of formula\\(e\\) not empty or string")
  }
})

test_that("globalize inverts localize", {
  for (chk in chks_gbl)
    expect_identical(chk, globalize(localize(chk)))
})

test_that("localize inverts globalize", {
  lcl_chkrs <- lapply(chks_gbl, localize)

  for (chkr in lcl_chkrs)
    expect_equal(chkr, localize(globalize(chkr)))
})

test_that("globalize(x) raises error when x not local checker", {
  # A local checker is a function of class "check_maker"
  nonlc <- list(
    structure(NULL, class = "check_maker"),
    `class<-`(localize("Not numeric" ~ is.numeric), NULL)
  )

  errmsg <- "`chkr` must be a local checker function"
  for (x in nonlc)
    expect_error_p(globalize(x), errmsg)
})

test_that("localize(x) raises error when x not global check formula", {
  errmsg <- "`chk` must be a formula of the form <string> ~ <predicate>"
  for (chk in chks_nongbl)
    expect_error_p(localize(chk), errmsg)
})
