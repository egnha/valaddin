context("Checklists")

test_that("is_check_formula() yields FALSE for invalid check formulas", {
  for (chk in bad_checks) {
    expect_false(is_checklist(list(chk)))

    out <- tryCatch(is_check_formula(chk), error = function(e) NA)
    if (!is.na(out))
      expect_false(is_check_formula(chk))
  }

})

test_that("is_check_formula() yields TRUE for valid check formulas", {
  good_checks <- list(
    ~ is.numeric,
    "Not numeric" ~ is.numeric,
    ~ {. > 0},
    "Not positive" ~ {. > 0},
    ~ function(x) {abs(x) > 0},
    "Not nonzero" ~ function(x) {abs(x) > 0},
    list(~x) ~ is.numeric,
    list(~x, ~y) ~ is.numeric,
    list(~x, "`y` not numeric" ~ y) ~ is.numeric,
    list("`x` not numeric" ~ x, "`y` not numeric" ~ y) ~ is.numeric,
    list(~list(x, y)) ~ purrr::lift(function(x, y) {x - y > 0})
  )

  for (chk in good_checks) {
    expect_true(is_check_formula(chk))
  }
})