context("Localized checkers")

checkers <- (
  getNamespaceExports("valaddin")
  %>% .[startsWith(., "vld_")]
  %>% lapply(getExportedValue, ns = "valaddin")
)

test_that("checker class is 'check_maker'", {
  for (chkr in checkers)
    expect_true(is_check_maker(chkr))
})

test_that("environment of check formula is package namespace environment", {
  pkg_ns_env <- getNamespace("valaddin")
  for (chkr in checkers)
    expect_identical(lazyeval::f_env(chkr(~x)), pkg_ns_env)
})

test_that("type checkers enforce type", {
  f <- firmly(identity, vld_character(~x))
  expect_equal(f(letters), letters)
  expect_error(f(0), "Not character: x")

  f <- firmly(identity, vld_complex(~x))
  expect_equal(f(1:3 + 1i), 1:3 + 1i)
  expect_error(f(1:3), "Not complex: x")

  f <- firmly(identity, vld_double(~x))
  expect_equal(f(1:3 + 0), 1:3 + 0)
  expect_error(f(1:3), "Not double: x")

  f <- firmly(identity, vld_integer(~x))
  expect_equal(f(1:3), 1:3)
  expect_error(f(1:3 + 0), "Not integer: x")

  f <- firmly(identity, vld_logical(~x))
  expect_equal(f(1:3 %% 2 == 0), 1:3 %% 2 == 0)
  expect_error(f(1:3 %% 2), "Not logical: x")

  f <- firmly(identity, vld_raw(~x))
  expect_equal(f(raw(3)), raw(3))
  expect_error(f(1:3), "Not raw: x")
})

test_that("scalar checkers enforce type and length 1", {
  f <- firmly(identity, vld_scalar_character(~x))
  expect_equal(f("a"), "a")
  expect_error(f(0), "Not scalar character: x")
  expect_error(f(letters), "Not scalar character: x")

  f <- firmly(identity, vld_scalar_complex(~x))
  expect_equal(f(1 + 1i), 1 + 1i)
  expect_error(f(1), "Not scalar complex: x")
  expect_error(f(1:3 + 1i), "Not scalar complex: x")

  f <- firmly(identity, vld_scalar_double(~x))
  expect_equal(f(0), 0)
  expect_error(f(0L), "Not scalar double: x")
  expect_error(f(0 + 1:3), "Not scalar double: x")

  f <- firmly(identity, vld_scalar_integer(~x))
  expect_equal(f(0L), 0L)
  expect_error(f(0), "Not scalar integer: x")
  expect_error(f(1:3), "Not scalar integer: x")

  f <- firmly(identity, vld_scalar_logical(~x))
  expect_equal(f(1 %% 2 == 0), 1 %% 2 == 0)
  expect_error(f(1 %% 2), "Not scalar logical: x")
  expect_error(f(1:3 %% 2 == 0), "Not scalar logical: x")

  f <- firmly(identity, vld_scalar_raw(~x))
  expect_equal(f(raw(1)), raw(1))
  expect_error(f(1), "Not scalar raw: x")
  expect_error(f(raw(32)), "Not scalar raw: x")

  f <- firmly(identity, vld_boolean(~x))
  expect_equal(f(1 %% 2 == 0), 1 %% 2 == 0)
  expect_error(f(1 %% 2), "Not boolean: x")
  expect_error(f(1:3 %% 2 == 0), "Not boolean: x")

  f <- firmly(identity, vld_number(~x))
  expect_equal(f(0), 0)
  expect_equal(f(0L), 0L)
  expect_error(f("a"), "Not number: x")
  expect_error(f(1:3), "Not number: x")
  expect_error(f(1:3 + 0), "Not number: x")

  f <- firmly(identity, vld_string(~x))
  expect_equal(f("a"), "a")
  expect_error(f(0), "Not string: x")
  expect_error(f(letters), "Not string: x")

  f <- firmly(identity, vld_scalar_atomic(~x))
  expect_equal(f(0), 0)
  expect_equal(f(0L), 0L)
  expect_equal(f(FALSE), FALSE)
  expect_equal(f("a"), "a")
  expect_equal(f(1 + 1i), 1 + 1i)
  expect_equal(f(raw(1)), raw(1))
  expect_error(f(NULL), "Not scalar atomic: x")
  expect_error(f(list(NULL)), "Not scalar atomic: x")
  expect_error(f(1:3), "Not scalar atomic: x")

  f <- firmly(identity, vld_scalar_list(~x))
  expect_equal(f(list(0)), list(0))
  expect_error(f(0), "Not scalar list: x")
  expect_error(f(list()), "Not scalar list: x")

  f <- firmly(identity, vld_scalar_numeric(~x))
  expect_equal(f(0), 0)
  expect_equal(f(0L), 0L)
  expect_error(f("a"), "Not scalar numeric: x")
  expect_error(f(1:3), "Not scalar numeric: x")
  expect_error(f(1:3 + 0), "Not scalar numeric: x")

  f <- firmly(identity, vld_scalar_vector(~x))
  expect_equal(f(0), 0)
  expect_equal(f(0L), 0L)
  expect_equal(f(FALSE), FALSE)
  expect_equal(f("a"), "a")
  expect_equal(f(1 + 1i), 1 + 1i)
  expect_equal(f(raw(1)), raw(1))
  expect_equal(f(list(NULL)), list(NULL))
  expect_error(f(NULL), "Not scalar vector: x")
  expect_error(f(1:3), "Not scalar vector: x")

  f <- firmly(identity, vld_singleton(~x))
  expect_equal(f(0), 0)
  expect_equal(f(list(0)), list(0))
  expect_error(f(1:3), "Not singleton: x")
  expect_error(f(NULL), "Not singleton: x")
  expect_error(f(as.list(1:3)), "Not singleton: x")
})

test_that("miscellaneous checkers enforce corresponding predicate", {
  f <- firmly(identity, vld_all(~x))
  expect_equal(f(1:3 > 0), 1:3 > 0)
  expect_error(f(0:3 > 0), "Not all TRUE: x")

  f <- firmly(identity, vld_any(~x))
  expect_equal(f(1:3 == 1), 1:3 == 1)
  expect_error(f(1:3 == 0), "None TRUE: x")

  f <- firmly(identity, vld_array(~x))
  expect_equal(f(array(1:3)), array(1:3))
  expect_error(f(1:3), "Not array: x")

  f <- firmly(identity, vld_atomic(~x))
  expect_equal(f(NULL), NULL)
  expect_error(f(list(NULL)), "Not atomic: x")

  f <- firmly(identity, vld_call(~x))
  expect_equal(f(quote(g())), quote(g()))
  expect_error(f(quote(g)), "Not call: x")

  f <- firmly(identity, vld_closure(~x))
  expect_equal(f(f), f)
  expect_error(f(log), "Not closure: x")  # special
  expect_error(f(`+`), "Not closure: x")  # builtin

  f <- firmly(identity, vld_data_frame(~x))
  expect_equal(f(data.frame(a = 0)), data.frame(a = 0))
  expect_error(f(list(a = 0)), "Not data frame: x")

  f <- firmly(identity, vld_empty(~x))
  expect_equal(f(list()), list())
  expect_error(f(list(NULL)), "Not empty: x")

  f <- firmly(identity, vld_environment(~x))
  expect_equal(f(getNamespace("base")), getNamespace("base"))
  expect_error(f(NULL), "Not environment: x")

  f <- firmly(identity, vld_expression(~x))
  expect_equal(f(expression(0)), expression(0))
  expect_error(f(quote(0)), "Not expression: x")

  f <- firmly(identity, vld_factor(~x))
  expect_equal(f(as.factor(1:3)), as.factor(1:3))
  expect_error(f(1:3), "Not factor: x")

  f <- firmly(identity, vld_false(~x))
  expect_false(f(FALSE))
  expect_error(f(TRUE), "Not FALSE: x")

  f <- firmly(identity, vld_formula(~x))
  expect_equal(f(a ~ b), a ~ b)
  expect_error(f(quote(a ~ b)), "Not formula: x")

  f <- firmly(identity, vld_function(~x))
  expect_equal(f(f), f)      # closure
  expect_equal(f(log), log)  # special
  expect_equal(f(`+`), `+`)  # builtin
  expect_error(f(NULL), "Not function: x")

  f <- firmly(identity, vld_language(~x))
  expect_equal(f(quote(1 + 2)), quote(1 + 2))
  expect_error(f(1 + 2), "Not language: x")

  f <- firmly(identity, vld_list(~x))
  expect_equal(f(list(NULL)), list(NULL))
  expect_error(f(NULL), "Not list: x")

  f <- firmly(identity, vld_matrix(~x))
  expect_equal(f(matrix(1:3)), matrix(1:3))
  expect_error(f(1:3), "Not matrix: x")

  f <- firmly(identity, vld_na(~x))
  expect_true(is.na(f(NA)))
  expect_error(f(0), "Not NA: x")

  f <- firmly(identity, vld_name(~x))
  expect_equal(f(as.name("a")), as.name("a"))
  expect_error(f("a"), "Not name: x")

  f <- firmly(identity, vld_nan(~x))
  expect_true(is.nan(f(NaN)))
  expect_error(f(NA), "Not NaN: x")

  f <- firmly(identity, vld_null(~x))
  expect_null(f(NULL))
  expect_error(f(NA), "Not null: x")

  f <- firmly(identity, vld_numeric(~x))
  expect_equal(f(0), 0)
  expect_equal(f(0L), 0L)
  expect_error(f(0 + 1i), "Not numeric: x")

  f <- firmly(identity, vld_ordered(~x))
  expect_equal(f(as.ordered(1:3)), as.ordered(1:3))
  expect_error(f(as.factor(1:3)), "Not ordered: x")

  f <- firmly(identity, vld_pairlist(~x))
  expect_equal(f(pairlist(a = 0)), pairlist(a = 0))
  expect_error(f(list(a = 0)), "Not pairlist: x")

  f <- firmly(identity, vld_primitive(~x))
  expect_equal(f(log), log)  # special
  expect_equal(f(`+`), `+`)  # builtin
  expect_error(f(f), "Not primitive: x")

  f <- firmly(identity, vld_recursive(~x))
  expect_equal(f(list(0)), list(0))
  expect_error(f(0), "Not recursive: x")

  f <- firmly(identity, vld_symbol(~x))
  expect_equal(f(as.symbol("a")), as.symbol("a"))
  expect_error(f("a"), "Not symbol: x")

  f <- firmly(identity, vld_table(~x))
  expect_equal(f(table(0)), table(0))
  expect_error(f(0), "Not table: x")

  f <- firmly(identity, vld_true(~x))
  expect_true(f(TRUE))
  expect_error(f(FALSE), "Not TRUE: x")

  f <- firmly(identity, vld_unsorted(~x))
  expect_equal(f(3:1), 3:1)
  expect_error(f(1:3), "Not unsorted: x")

  f <- firmly(identity, vld_vector(~x))
  expect_equal(f(list(NULL)), list(NULL))
  expect_equal(f(0), 0)
  expect_error(f(NULL), "Not vector: x")
})
