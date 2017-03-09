context("Localized checkers")

nms <- list()
nms$vld <- grep("^vld_", getNamespaceExports("valaddin"), value = TRUE)
nms_is <- sub("^vld_", "is_", nms$vld)
purrr_pred <- grep("^is_", getNamespaceExports("purrr"), value = TRUE)
in_purrr <- nms_is %in% purrr_pred
nms$purrr <- nms_is[in_purrr]
nms$base  <- nms$vld[!in_purrr] %>%
  sub("^vld_", "is.", .) %>%
  gsub("_", ".", .)

chkrs <- purrr::transpose(
  list(
    chkr = nms$vld %>%
      {c(.[in_purrr], .[!in_purrr])} %>%
      lapply(getExportedValue, ns = "valaddin"),
    nm = c(nms$purrr, nms$base),
    ns = c("purrr", "base") %>%
      lapply(function(.) rep(., length(nms[[.]]))) %>%
      unlist()
  )
)

aliases <- paste0("vld_", c("boolean", "number", "string", "singleton"))
chkrs_orig <- chkrs[!names(chkrs) %in% aliases]

test_that("checker class is 'check_maker'", {
  for (. in chkrs)
    expect_true(is_check_maker(.$chkr))
})

test_that("checker predicate matches underlying predicate function", {
  for (. in chkrs_orig) {
    pred_chkr <- ff_eval_rhs(globalize(.$chkr))
    pred_orig <- getExportedValue(.$ns, .$nm)

    expect_identical(pred_chkr, pred_orig)
  }
})

test_that("checker error message is derived from predicate name", {
  f <- function(x) NULL

  for (. in chkrs_orig) {
    msg_chkr <- ff_eval_lhs(globalize(.$chkr))
    msg <- sprintf("Not %s", gsub("[_\\.]", " ", substring(.$nm, 4L)))
    expect_identical(msg_chkr, msg)

    # Every purrr, resp. base, predicate returns FALSE for log, resp. 0L
    bad_arg <- if (.$ns == "purrr") log else 0L
    expect_error(firmly(f, .$chkr(~ x))(bad_arg), msg)
  }
})
