#' @include scope.R
NULL

is_prim <- function(nm, pkg = "package:base") {
  is.primitive(get(nm, pos = pkg))
}

localize_obj <- function(obj, root, env, pkg = "package:base") {
  msg <- paste("Not", root)
  p <- get(obj, pos = pkg)

  localize_check(lazyeval::f_new(p, msg, env = env))
}

base <- objects("package:base", all.names = TRUE)
prims <- base[vapply(base, is_prim, logical(1))]
preds <- prims[grep("^is[.]", prims)]
names(preds) <- gsub("^is[.]", "", preds)

chkrs <- purrr::map2(preds, names(preds), localize_obj, env = parent.frame())
names(chkrs) <- paste("vld", names(preds), sep = "_")

for (nm in names(chkrs)) assign(nm, chkrs[[nm]])

#' #' @rawNamespace exportPattern("^vld_.*$")
#' NULL