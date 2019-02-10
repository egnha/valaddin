
<!-- README.md is generated from README.Rmd. Please edit that file -->

**Development has moved to [rong](https://github.com/egnha/rong)**

# valaddin

[![Build
Status](https://travis-ci.org/egnha/valaddin.svg?branch=1.0.0)](https://travis-ci.org/egnha/valaddin)
[![codecov](https://codecov.io/gh/egnha/valaddin/branch/1.0.0/graph/badge.svg)](https://codecov.io/gh/egnha/valaddin/branch/1.0.0)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/valaddin)](https://cran.r-project.org/package=valaddin)
[![stability-frozen](https://img.shields.io/badge/stability-frozen-blue.svg)](https://github.com/emersion/stability-badges#frozen)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Dealing with invalid function inputs is a chronic pain for R users,
given R’s weakly typed nature. *valaddin* provides pain relief—a
lightweight R package that enables you to transform an existing function
into a function with input validation checks, *in situ*, in a manner
suitable for both programmatic use and interactive sessions.

## Installation

Install from [CRAN](https://cran.r-project.org/package=valaddin)

``` r
install.packages("valaddin")
```

or get the development version from GitHub using the
[devtools](https://github.com/hadley/devtools) package

``` r
# install.packages("devtools")
devtools::install_github("egnha/valaddin")
```

## Why use valaddin

### Fail fast—save time, spare confusion

You can be more confident your function works correctly, when you know
its arguments are well-behaved. But when they aren’t, its better to stop
immediately and bring them into line, than to let them pass and wreak
havoc, exposing yourself to breakages or, worse, silently incorrect
results. Validating the inputs of your functions is good [defensive
programming](http://adv-r.had.co.nz/Exceptions-Debugging.html#defensive-programming)
practice.

Suppose you have a function `secant()`

``` r
secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
```

and you want to ensure that the user (or some code) supplies numerical
inputs for `x` and `dx`. Typically, you’d rewrite `secant()` so that it
stops if this condition is violated:

``` r
secant_numeric <- function(f, x, dx) {
  stopifnot(is.numeric(x), is.numeric(dx))
  secant(f, x, dx)
}

secant_numeric(log, 1, .1)
#> [1] 0.9531018

secant_numeric(log, "1", ".1")
#> Error in secant_numeric(log, "1", ".1"): is.numeric(x) is not TRUE
```

### The standard approach in R is problematic

While this works, it’s not ideal, even in this simple situation, because

  - it’s inconvenient for interactive use at the console: you have to
    declare a new function, and give it a new name (or copy-paste the
    original function body)

  - it doesn’t catch all errors, only the first that occurs among the
    checks

  - you’re back to square one, if you later realize you need additional
    checks, or want to skip them altogether.

### valaddin rectifies these shortcomings

valaddin provides a function `firmly()` that takes care of input
validation by *transforming* the existing function, instead of forcing
you to write a new one. It also helps you by reporting *every* failing
check.

``` r
library(valaddin)

# Check that `x` and `dx` are numeric
secant <- firmly(secant, list(~x, ~dx) ~ is.numeric)

secant(log, 1, .1)
#> [1] 0.9531018

secant(log, "1", ".1")
#> Error: secant(f = log, x = "1", dx = ".1")
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: is.numeric(dx)
```

To add additional checks, just apply the same procedure again:

``` r
secant <- firmly(secant, list(~x, ~dx) ~ {length(.) == 1L})

secant(log, "1", c(.1, .01))
#> Error: secant(f = log, x = "1", dx = c(0.1, 0.01))
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: (function(.) {length(.) == 1L})(dx)
```

Or, alternatively, all in one go:

``` r
secant <- loosely(secant)  # Retrieves the original function
secant <- firmly(secant, list(~x, ~dx) ~ {is.numeric(.) && length(.) == 1L})

secant(log, 1, .1)
#> [1] 0.9531018

secant(log, "1", c(.1, .01))
#> Error: secant(f = log, x = "1", dx = c(0.1, 0.01))
#> 1) FALSE: (function(.) {is.numeric(.) && length(.) == 1L})(x)
#> 2) FALSE: (function(.) {is.numeric(.) && length(.) == 1L})(dx)
```

### Check anything using a simple, consistent syntax

`firmly()` uses a simple formula syntax to specify arbitrary checks—not
just type checks. Every check is a formula of the form `<where to check>
~ <what to check>`. The “what” part on the right is a *function* that
does a check, while the (form of the) “where” part on the left indicates
where to apply the check—at which *arguments* or *expressions* thereof.

valaddin provides a number of conveniences to make checks for `firmly()`
informative and easy to specify.

#### Use custom error messages

Use a custom error message to clarify the *purpose* of a check:

``` r
bc <- function(x, y) c(x, y, 1 - x - y)

# Check that `y` is positive
bc_uhp <- firmly(bc, list("(x, y) not in upper half-plane" ~ y) ~ {. > 0})

bc_uhp(.5, .2)
#> [1] 0.5 0.2 0.3

bc_uhp(.5, -.2)
#> Error: bc_uhp(x = 0.5, y = -0.2)
#> (x, y) not in upper half-plane
```

#### Easily apply a check to all arguments

Leave the left-hand side of a check formula blank to apply it to all
arguments:

``` r
bc_num <- firmly(bc, ~is.numeric)

bc_num(.5, ".2")
#> Error: bc_num(x = 0.5, y = ".2")
#> FALSE: is.numeric(y)

bc_num(".5", ".2")
#> Error: bc_num(x = ".5", y = ".2")
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: is.numeric(y)
```

Or fill in a custom error message:

``` r
bc_num <- firmly(bc, "Not numeric" ~ is.numeric)

bc_num(.5, ".2")
#> Error: bc_num(x = 0.5, y = ".2")
#> Not numeric: `y`
```

#### Check conditions with multi-argument dependencies

Use the `isTRUE()` predicate to implement checks depending on multiple
arguments or, equivalently, the check maker `vld_true()`:

``` r
in_triangle <- function(x, y) {x >= 0 && y >= 0 && 1 - x - y >= 0}
outside <- "(x, y) not in triangle"

bc_tri <- firmly(bc, list(outside ~ in_triangle(x, y)) ~ isTRUE)

# Or more concisely:
bc_tri <- firmly(bc, vld_true(outside ~ in_triangle(x, y)))

# Or more concisely still, by relying on an auto-generated error message:
# bc_tri <- firmly(bc, vld_true(~in_triangle(x, y)))

bc_tri(.5, .2)
#> [1] 0.5 0.2 0.3

bc_tri(.5, .6)
#> Error: bc_tri(x = 0.5, y = 0.6)
#> (x, y) not in triangle
```

Alternatively, use the `lift()` function from the
[purrr](https://github.com/hadley/purrr)
package:

``` r
bc_tri <- firmly(bc, list(outside ~ list(x, y)) ~  purrr::lift(in_triangle))
```

### Make your code more intelligible

To make your functions more intelligible, declare your input assumptions
and move the core logic to the fore. You can do this using `firmly()`,
in several ways:

  - Precede the function header with input checks, by explicitly
    assigning the function to `firmly()`’s `.f` argument:
    
    ``` r
    bc <- firmly(
      ~is.numeric,
      ~{length(.) == 1L},
      vld_true(outside ~ in_triangle(x, y)),
      .f = function(x, y) {
        c(x, y, 1 - x - y)
      }
    )
    
    bc(.5, .2)
    #> [1] 0.5 0.2 0.3
    
    bc(.5, c(.2, .1))
    #> Error: bc(x = 0.5, y = c(0.2, 0.1))
    #> FALSE: (function(.) {length(.) == 1L})(y)
    
    bc(".5", 1)
    #> Error: bc(x = ".5", y = 1)
    #> 1) FALSE: is.numeric(x)
    #> 2) (x, y) not in triangle
    ```

  - Use the [magrittr](https://github.com/tidyverse/magrittr) `%>%`
    operator to deliver input checks, by capturing them as a list with
    `firmly()`’s `.checklist` argument:
    
    ``` r
    library(magrittr)
    
    bc2 <- list(
      ~is.numeric,
      ~{length(.) == 1L},
      vld_true(outside ~ in_triangle(x, y))
    ) %>%
      firmly(function(x, y) {
        c(x, y, 1 - x - y)
      },
      .checklist = .)
    
    all.equal(bc, bc2)
    #> [1] TRUE
    ```

  - Better yet, use the `%checkin%` operator:
    
    ``` r
    bc3 <- list(
      ~is.numeric,
      ~{length(.) == 1L},
      vld_true(outside ~ in_triangle(x, y))
    ) %checkin%
      function(x, y) {
        c(x, y, 1 - x - y)
      }
    
    all.equal(bc, bc3)
    #> [1] TRUE
    ```

### Learn more

See the package documentation `?firmly`, `help(p = valaddin)` for
detailed information about `firmly()` and its companion functions, and
the
[vignette](https://cran.r-project.org/package=valaddin/vignettes/valaddin.html)
for an overview of use cases.

## Related packages

  - [assertive](https://bitbucket.org/richierocks/assertive),
    [assertthat](https://github.com/hadley/assertthat), and
    [checkmate](https://github.com/mllg/checkmate) provide handy
    collections of predicate functions that you can use in conjunction
    with `firmly()`.

  - [argufy](https://github.com/gaborcsardi/argufy) takes a different
    approach to input validation, using
    [roxygen](https://github.com/klutometis/roxygen) comments to specify
    checks.

  - [ensurer](https://github.com/smbache/ensurer) and
    [assertr](https://github.com/ropensci/assertr) provide a means of
    validating function values. Additionally, ensurer provides an
    experimental replacement for `function()` that builds functions with
    type-validated arguments.

  - [typeCheck](https://github.com/jimhester/typeCheck), together with
    [Types for R](https://github.com/jimhester/types), enables the
    creation of functions with type-validated arguments by means of
    special type annotations. This approach is orthogonal to that of
    valaddin: whereas valaddin specifies input checks as *predicate
    functions with scope*, typeCheck specifies input checks as
    *arguments with type*.

## License

MIT Copyright © 2019 [Eugene Ha](https://github.com/egnha)
