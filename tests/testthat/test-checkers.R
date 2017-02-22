context("Checkers")

test_that("localized purrr checkers implement corresponding purrr predicates", {
  prefix <- "^vld_"
  nms_valaddin <- grep(prefix, getNamespaceExports("valaddin"), value = TRUE)
  nms_purrr <- sub(prefix, "is_", nms_valaddin)
  nms <- purrr::transpose(list(valaddin = nms_valaddin, purrr = nms_purrr))
  preds <- lapply(nms, function(.) {
    lcl <- getExportedValue("valaddin", .$valaddin)
    list(
      valaddin = lazyeval::f_eval_rhs(globalize(lcl)),
      purrr    = getExportedValue("purrr", .$purrr)
    )
  })

  for (p in preds)
    expect_identical(p$valaddin, p$purrr)
})
