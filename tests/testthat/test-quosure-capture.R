context("Capturing input validation checks")

test_that("definitions are encoded as message-check quosure pairs", {
  expect_equivalent(
    vld(a := b),
    list(
      list(msg = rlang::quo(a), chk = rlang::quo(b))
    )
  )
  expect_equivalent(
    vld(a := b, c := d),
    list(
      list(msg = rlang::quo(a), chk = rlang::quo(b)),
      list(msg = rlang::quo(c), chk = rlang::quo(d))
    )
  )
})

test_that("checks without LHS are encoded as empty string-check pairs", {
  expect_equivalent(
    vld(x),
    list(
      list(msg = rlang::quo(""), chk = rlang::quo(x))
    )
  )
  expect_equivalent(
    vld(x, y),
    list(
      list(msg = rlang::quo(""), chk = rlang::quo(x)),
      list(msg = rlang::quo(""), chk = rlang::quo(y))
    )
  )
  expect_equivalent(
    vld(x, y, a := b),
    list(
      list(msg = rlang::quo(""), chk = rlang::quo(x)),
      list(msg = rlang::quo(""), chk = rlang::quo(y)),
      list(msg = rlang::quo(a), chk = rlang::quo(b))
    )
  )
})

test_that("vld() can be spliced into vld() with !!!/UQS", {
  expect_equivalent(
    vld(x, !!! vld(a := b, y), c := d),
    vld(x, a := b, y, c := d)
  )
  expect_equivalent(
    vld(x, UQS(vld(a := b, y)), c := d),
    vld(x, a := b, y, c := d)
  )
})

test_that("unquoting is supported", {
  x <- "x"
  y <- "y"
  expect_equivalent(
    vld(!! x := y, !! x, x := !! y, !! x := !! y),
    vld("x" := y, "x", x := "y", "x" := "y")
  )
})
