## ---- include = FALSE----------------------------------------------------
library(valaddin)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
f <- function(x, h) (sin(x + h) - sin(x)) / h

## ------------------------------------------------------------------------
ff <- firmly(f, ~ is.numeric)

## ------------------------------------------------------------------------
nearly_cos <-
  firmly(f, ~ is.numeric, list("abs(h) is too big" ~ abs(h)) ~ {. > 0 && . < 1})

## ------------------------------------------------------------------------
Sys.setenv(TZ = "CET")
(d <- as.POSIXct("2017-01-01 09:30:00"))

## ------------------------------------------------------------------------
as.POSIXlt(d, tz = "EST")$hour

## ------------------------------------------------------------------------
Sys.setenv(TZ = "EST")
d <- as.POSIXct("2017-01-01 09:30:00")
as.POSIXlt(d, tz = "EST")$hour

## ---- include = FALSE----------------------------------------------------
Sys.setenv(TZ = "CET")

## ------------------------------------------------------------------------
as.POSIXct <- firmly(as.POSIXct, .warn_missing = "tz")

## ------------------------------------------------------------------------
as.POSIXct("2017-01-01 09:30:00")

as.POSIXct("2017-01-01 09:30:00", tz = "CET")

## ------------------------------------------------------------------------
loosely(as.POSIXct)("2017-01-01 09:30:00")

identical(loosely(as.POSIXct), base::as.POSIXct)

## ------------------------------------------------------------------------
w <- {set.seed(1); rnorm(5)}

ifelse(w > 0, 0, w)

## ------------------------------------------------------------------------
z <- rep(1, 6)
pos <- 1:5
neg <- -6:-1

ifelse(z > 0, pos, neg)

## ------------------------------------------------------------------------
chk_length_type <- list(
  "'yes', 'no' differ in length" ~ length(yes) == length(no),
  "'yes', 'no' differ in type" ~ typeof(yes) == typeof(no)
) ~ isTRUE

ifelse_f <- firmly(ifelse, chk_length_type)

## ------------------------------------------------------------------------
deposit <- function(account, value) {
  if (is_student(account)) {
    account$fees <- 0
  }
  account$balance <- account$balance + value
  account
}

is_student <- function(account) {
  if (isTRUE(account$is_student)) TRUE else FALSE
}

## ------------------------------------------------------------------------
bobs_acct <- list(balance = 10, fees = 3, is_student = FALSE)

## ------------------------------------------------------------------------
deposit(bobs_acct, bobs_acct$fees)$balance

## ------------------------------------------------------------------------
bobs_acct$is_student <- TRUE

## ------------------------------------------------------------------------
bobs_acct <- list2env(bobs_acct)

## ------------------------------------------------------------------------
deposit(bobs_acct, bobs_acct$fees)$balance

## ------------------------------------------------------------------------
err_msg <- "`acccount` should not be an environment"
deposit <- firmly(deposit, list(err_msg ~ account) ~ Negate(is.environment))

## ------------------------------------------------------------------------
x <- "An expensive object"
save(x, file = "my-precious.rda")

x <- "Oops! A bug or lapse has tarnished your expensive object"

# Many computations later, you again save x, oblivious to the accident ...
save(x, file = "my-precious.rda")

## ------------------------------------------------------------------------
# Argument `gear` is a list with components:
# fun: Function name
# ns : Namespace of `fun`
# chk: Formula that specify input checks

hardhat <- function(gear, env = .GlobalEnv) {
  for (. in gear) {
    safe_fun <- firmly(getFromNamespace(.$fun, .$ns), .$chk)
    assign(.$fun, safe_fun, envir = env)
  }
}

## ------------------------------------------------------------------------
protection <- list(
  list(
    fun = "save",
    ns  = "base",
    chk = list("Won't overwrite `file`" ~ file) ~ Negate(file.exists)
  ),
  list(
    fun = "load",
    ns  = "base",
    chk = list("Won't load objects into current environment" ~ envir) ~
      {!identical(., parent.frame(2))}
  )
)

## ------------------------------------------------------------------------
hardhat(protection)

## ---- include = FALSE----------------------------------------------------
file.remove("my-precious.rda")

## ---- include = FALSE----------------------------------------------------
file.remove("my-precious.rda")

