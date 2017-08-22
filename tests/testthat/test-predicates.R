context("Predicates")

expect_pass_fail <- function(chkr, pass, fail) {
  expr <- substitute(chkr)

  data <- list()
  data$. <- "x"
  data$.value <- rlang::node_cdr(expr) %||% list(NULL)
  is_chr <- vapply(data$.value, is.character, logical(1))
  data$.value[is_chr] <-
    esc_perl(encodeString(data$.value[is_chr], quote = "\""))
  data$.value[!is_chr] <- lapply(data$.value[!is_chr], deparse_collapse)
  data$.expr <- data$.value

  msg <- chkr_message(eval.parent(expr[[1]]))
  msg <- glue::glue_data(.x = data, msg, .open = "{{", .close = "}}")

  expect_error(firmly(function(x) NULL, !! expr)(pass), NA)
  expect_error(firmly(function(x) NULL, !! expr)(fail), msg, perl = TRUE)
}

test_that("boolean predicates work", {
  expect_pass_fail(vld_true(), TRUE, FALSE)
  expect_pass_fail(vld_is(), TRUE, FALSE)
  expect_pass_fail(vld_false(), FALSE, TRUE)
  expect_pass_fail(vld_not(), FALSE, TRUE)
  expect_pass_fail(vld_all(), c(TRUE, TRUE), c(FALSE, TRUE))
  expect_pass_fail(vld_all(na.rm = TRUE), c(TRUE, NA), c(FALSE, NA))
  expect_pass_fail(vld_any(), c(TRUE, FALSE), c(FALSE, FALSE))
  expect_pass_fail(vld_any(na.rm = TRUE), c(TRUE, NA), c(FALSE, NA))
  expect_pass_fail(vld_none(), c(FALSE, FALSE), c(FALSE, TRUE))
  expect_pass_fail(vld_none(na.rm = TRUE), c(FALSE, NA), c(TRUE, NA))
  expect_pass_fail(vld_all_map(f = `!`), c(FALSE, FALSE), c(TRUE, FALSE))
  expect_pass_fail(vld_all_map(f = `!`, na.rm = TRUE), c(FALSE, NA), c(TRUE, NA))
  expect_pass_fail(vld_any_map(f = `!`), c(FALSE, TRUE), c(TRUE, TRUE))
  expect_pass_fail(vld_any_map(f = `!`, na.rm = TRUE), c(FALSE, NA), c(TRUE, NA))
  expect_pass_fail(vld_none_map(f = `!`), c(TRUE, TRUE), c(FALSE, FALSE))
  expect_pass_fail(vld_none_map(f = `!`, na.rm = TRUE), c(TRUE, NA), c(FALSE, NA))
})

test_that("object predicates work", {
  expect_pass_fail(vld_call(), quote(f(x)), NULL)
  expect_pass_fail(vld_factor(), as.factor(letters), NULL)
  expect_pass_fail(vld_data_frame(), data.frame(), NULL)
  expect_pass_fail(vld_matrix(), matrix(), NULL)
  expect_pass_fail(vld_formula(), x ~ y, NULL)
  expect_pass_fail(vld_function(), log, NULL)
})

test_that("pattern predicates work", {
  expect_pass_fail(vld_grepl(pattern = "^this"),
                   "this...",
                   "that")
  expect_pass_fail(vld_grepl(pattern = "^this", ignore.case = TRUE),
                   "tHiS...",
                   "that")
  expect_pass_fail(vld_grepl(pattern = "^this\\(", perl = TRUE),
                   "this(...",
                   "that(")
  expect_pass_fail(vld_starts_with(prefix = "this"), "this...", "that...")
  expect_pass_fail(vld_starts_with(prefix = "this", na.rm = TRUE),
                   c("this...", NA),
                   c("that...", NA))
  expect_pass_fail(vld_ends_with(suffix = "this"), "...this", "...that")
  expect_pass_fail(vld_ends_with(suffix = "this", na.rm = TRUE),
                   c("...this", NA),
                   c("...that", NA))
})

test_that("property predicates work", {
  attrs <- function(x)
    do.call("structure", c(NA, as.list(set_names(x))))

  expect_pass_fail(vld_empty(), list(), 0)
  expect_pass_fail(vld_not_empty(), 0, list())
  expect_pass_fail(vld_singleton(), 0, letters)
  expect_pass_fail(vld_not_na(), NULL, NA)
  expect_pass_fail(vld_without_na(), c(TRUE, FALSE), c(TRUE, NA))
  expect_pass_fail(vld_named(), c(a = "a"), "a")
  expect_pass_fail(vld_has_name(nm = "a"), c(a = "a", "b"), c(A = "a", "b"))
  expect_pass_fail(vld_has_names(nms = letters), set_names(letters), letters)
  expect_pass_fail(vld_has_length(n = 1), 1, 1:2)
  expect_pass_fail(vld_has_attr(which = "a"), structure(NA, a = "a"), NA)
  expect_pass_fail(vld_has_attrs(which = letters), attrs(letters), attrs("a"))
  expect_pass_fail(vld_inherits(what = "Class"), `class<-`(NA, "Class"), NA)
})

test_that("relational predicates work", {
  expect_pass_fail(vld_identical(to = 0L), 0L, 0.0)
  expect_pass_fail(vld_not_identical(to = 0L), 0.0, 0L)
  expect_pass_fail(vld_equal(to = 0), 0, 1)
  expect_pass_fail(vld_not_equal(to = 0), 1, 0)
  expect_pass_fail(vld_equivalent(to = 0), structure(0, a = "a"), 1)
  expect_pass_fail(vld_not_equivalent(to = 0), 1, structure(0, a = "a"))
  expect_pass_fail(vld_gt(lwr = 0), 1, -1)
  expect_pass_fail(vld_gt(lwr = 0, na.rm = TRUE), c(1, NA), c(-1, NA))
  expect_pass_fail(vld_lt(upr = 0), -1, 1)
  expect_pass_fail(vld_lt(upr = 0, na.rm = TRUE), c(-1, NA), c(1, NA))
  expect_pass_fail(vld_gte(lwr = 0), 0, -1)
  expect_pass_fail(vld_gte(lwr = 0, na.rm = TRUE), c(0, NA), c(-1, NA))
  expect_pass_fail(vld_lte(upr = 0, na.rm = TRUE), c(0, NA), c(1, NA))
})

test_that("set predicates work", {
  expect_pass_fail(vld_in(set = letters), "a", "A")
  expect_pass_fail(vld_not_in(set = letters), "A", "a")
  expect_pass_fail(vld_include(set = "a"), letters, letters[-1])
  expect_pass_fail(vld_exclude(set = "a"), letters[-1], letters)
  expect_pass_fail(vld_within(set = letters), c("a", "b"), c("a", "b", "C"))
  expect_pass_fail(vld_avoid(set = "a"), letters[-1], letters)
  expect_pass_fail(vld_setequal(set = letters), rev(letters), letters[-1])
})

test_that("type predicates work", {
  expect_pass_fail(vld_null(), NULL, 0)
  expect_pass_fail(vld_not_null(), 0, NULL)
  expect_pass_fail(vld_symbol(), as.symbol("x"), "x")
  expect_pass_fail(vld_pairlist(), formals(function(x = 0) NULL), list(x = 0))
  expect_pass_fail(vld_closure(), function(x) NULL, log)
  expect_pass_fail(vld_environment(), emptyenv(), list())
  expect_pass_fail(vld_language(), quote(f(x)), NULL)
  expect_pass_fail(vld_atomic(), 1:2, as.list(1:2))
  expect_pass_fail(vld_atomic(n = 1), 1, 1:2)
  expect_pass_fail(vld_vector(), vector(), NULL)
  expect_pass_fail(vld_vector(n = 1), vector(length = 1), vector(length = 2))
  expect_pass_fail(vld_logical(), logical(), NULL)
  expect_pass_fail(vld_logical(n = 1), logical(1), logical(2))
  expect_pass_fail(vld_boolean(), TRUE, NA)
  expect_pass_fail(vld_numerical(), numeric(), NULL)
  expect_pass_fail(vld_numerical(n = 1), numeric(1), numeric(2))
  expect_pass_fail(vld_number(), numeric(1), NA_real_)
  expect_pass_fail(vld_integer(), integer(), NULL)
  expect_pass_fail(vld_integer(n = 1), integer(1), integer(2))
  expect_pass_fail(vld_integerish(), c(1.0, 2.0), 0.000001)
  expect_pass_fail(vld_integerish(n = 1), 1.0, c(1.0, 2.0))
  expect_pass_fail(vld_double(), double(), NULL)
  expect_pass_fail(vld_double(n = 1), double(1), double(2))
  expect_pass_fail(vld_complex(), complex(), NULL)
  expect_pass_fail(vld_complex(n = 1), complex(1), complex(2))
  expect_pass_fail(vld_character(), character(), NULL)
  expect_pass_fail(vld_character(n = 1), character(1), character(2))
  expect_pass_fail(vld_string(), character(1), NA_character_)
  expect_pass_fail(vld_list(), list(), NULL)
  expect_pass_fail(vld_list(n = 1), vector("list", 1), vector("list", 2))
  expect_pass_fail(vld_raw(), raw(), NULL)
  expect_pass_fail(vld_raw(n = 1), raw(1), raw(2))
})
