context("Localized checkers")

prefix <- "^vld_"
nms_valaddin <- grep(prefix, getNamespaceExports("valaddin"), value = TRUE)
nms_purrr <- sub(prefix, "is_", nms_valaddin)
chkrs <- purrr::transpose(
  list(
    chkr = lapply(nms_valaddin, getExportedValue, ns = "valaddin"),
    nm   = nms_purrr
  )
)

test_that("purrr checker class is 'check_maker'", {
  check_maker <- vapply(chkrs, function(.) is_check_maker(.$chkr), logical(1))
  expect_true(all(check_maker))
})

test_that("purrr checker predicate is corresponding purrr predicate", {
  for (. in chkrs) {
    pred_chkr  <- lazyeval::f_eval_rhs(globalize(.$chkr))
    pred_purrr <- getExportedValue("purrr", .$nm)

    expect_identical(pred_chkr, pred_purrr)
  }
})

test_that("purrr checker error message is derived from purrr predicate name", {
  f <- function(x) NULL
  bad_arg <- log  # Every purrr predicate returns FALSE for bad_arg

  for (. in chkrs) {
    msg_chkr  <- lazyeval::f_eval_lhs(globalize(.$chkr))
    msg_purrr <- sprintf("Not %s", gsub("_", " ", sub("^is_", "", .$nm)))

    expect_identical(msg_chkr, msg_purrr)
    expect_error(strictly(f, .$chkr(x))(bad_arg), msg_purrr)
  }
})
