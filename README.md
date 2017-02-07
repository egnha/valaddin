# valaddin

[![Travis-CI Build Status](https://travis-ci.org/egnha/valaddin.svg?branch=master)](https://travis-ci.org/egnha/valaddin)
[![codecov](https://codecov.io/gh/egnha/valaddin/branch/master/graph/badge.svg)](https://codecov.io/gh/egnha/valaddin)
 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Dealing with invalid function inputs is a chronic pain for R users, given R's 
weakly typed nature. _valaddin_ provides pain relief: it is a simple R package 
that enables you to transform an existing function into a function with input
validation checks—with minimal fuss—in a manner suitable for both programmatic
use and interactive sessions.

## Why use valaddin

### Defensive programming is good practice

Validating the inputs of your functions is [good programming
practice](http://adv-r.had.co.nz/Exceptions-Debugging.html#defensive-programming).
Say you have a function `secant()` declared as follows:

```R
secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx
```

If you want to ensure that `secant()` only accepts numerical inputs for `x` and
`dx`, you'd normally rewrite `secant()` to check this condition, and stop if
it's violated:

```R
secant_numeric <- function(f, x, dx) {
  stopifnot(is.numeric(x), is.numeric(dx))
  secant(f, x, dx)
}

secant_numeric(log, 1, .1)
#> [1] 0.9531018

secant_numeric(log, "1", ".1")
#> Error: is.numeric(x) is not TRUE
```

### But the standard approach in R is problematic

While this works, it's not ideal, even in this simple situation, because

* it's inconvenient for interactive use at the console: you have to declare a 
new function, and give it a new name (or copy-paste the original function body)

* it doesn't catch all errors, only the first that occurs among the checks.

Moreover, if you later realize you need additional checks, for example, that `x`
and `dx` should be numbers not vectors, or want to skip them altogether, you're
back to square one:

```R
secant_scalar <- function(f, x, dx) {
  stopifnot(is.numeric(x), is.numeric(dx), length(x) == 1L, length(dx) == 1L)
  secant(f, x, dx)
}

secant_scalar(log, 1, c(.1, .01))
#> Error: length(dx) == 1L is not TRUE

secant_scalar(log, "1", c(.1, .01))  # Two problems, but only one is reported
#> Error: is.numeric(x) is not TRUE
```

### `strictly()` rectifies these shortcomings

valaddin provides a function `strictly()` that takes care of input validation by
_transforming_ the existing function, instead of forcing you to write a new one.
It also helps you by reporting _every_ failing check.

```R
library(valaddin)

# Check that `x` and `dx` are numeric
secant <- strictly(secant, list(~x, ~dx) ~ is.numeric)

secant(log, 1, .1)
#> [1] 0.9531018

secant(log, "1", ".1")
#> Error: secant(f = log, x = "1", dx = ".1")
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: is.numeric(dx)
```

To add additional checks, just apply the same procedure again:

```R
secant <- strictly(secant, list(~x, ~dx) ~ {length(.) == 1L})

secant(log, "1", c(.1, .01))
#> Error: secant(f = log, x = "1", dx = c(0.1, 0.01))
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: (function(.) {length(.) == 1L})(dx)
```

Or, alternatively, all in one go:

```R
secant <- nonstrictly(secant)  # Get back the original function
secant <- strictly(secant, list(~x, ~dx) ~ {is.numeric(.) && length(.) == 1L})

secant(log, 1, .1)
#> [1] 0.9531018

secant(log, "1", c(.1, .01))
#> Error: secant(f = log, x = "1", dx = c(0.1, 0.01))
#> 1) FALSE: (function(.) {is.numeric(.) && length(.) == 1L})(x)
#> 2) FALSE: (function(.) {is.numeric(.) && length(.) == 1L})(dx)
```

### Check anything using a simple, consistent syntax

`strictly()` uses a simple formula syntax to specify arbitrary checks—not just 
type checks. Every check is a formula of the form `<where to check> ~ <what to 
check>`. The "what" part on the right is a _function_ that does a check, while
the (form of the) "where" part on the left indicates where to apply the
check—at which _arguments_ or _expressions_ thereof.

valaddin provides a number of conveniences to make checks for `strictly()`
informative and easy to specify.

#### Use custom error messages

Use a custom error message to clarify the _purpose_ of a check:

```R
bc <- function(x, y) c(x, y, 1 - x - y)

# Check that `y` is positive
bc_uhp <- strictly(bc, list("(x, y) not in upper half-plane" ~ y) ~ {. > 0})

bc_uhp(.5, .2)
#> [1] 0.5 0.2 0.3

bc_uhp(.5, -.2)
#> Error: bc_uhp(x = 0.5, y = -0.2)
#> (x, y) not in upper half-plane
```

#### Easily apply a check to all arguments

Leave the left-hand side of a check formula blank to apply it to all arguments:

```R
bc_num <- strictly(bc, ~ is.numeric)

bc_num(.5, ".2")
#> Error: bc_num(x = 0.5, y = ".2")
#> FALSE: is.numeric(y)

bc_num(".5", ".2")
#> Error: bc_num(x = ".5", y = ".2")
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: is.numeric(y)
```

Or fill in a custom error message:

```R
bc_num <- strictly(bc, "Not numeric" ~ is.numeric)

bc_num(.5, ".2")
#> Error: bc_num(x = 0.5, y = ".2")
#> Not numeric: `y`
```

#### Check conditions with multi-argument dependencies

Use the `lift()` function from the [purrr](https://github.com/hadley/purrr)
package to specify checks depending on multiple arguments:

```R
library(purrr)

in_triangle <- function(x, y) x >= 0 && y >= 0 && 1 - x - y >= 0
in_triangle <- purrr::lift(in_triangle)
bc_tri <- strictly(bc, list("(x, y) not in triangle" ~ list(x, y)) ~ in_triangle)

bc_tri(.5, .2)
#> [1] 0.5 0.2 0.3

bc_tri(.5, .6)
#> Error: bc_tri(x = 0.5, y = 0.6)
#> (x, y) not in triangle
```

#### Layer checks using the magrittr pipe `%>%`

Activate checks in stages using the
[magrittr](https://github.com/tidyverse/magrittr) pipe `%>%`:

```R
library(magrittr)

bc <- bc %>%
  strictly("Not numeric" ~ is.numeric, "Not scalar" ~ {length(.) == 1L}) %>%
  strictly(list("(x, y) not in triangle" ~ list(x, y)) ~ in_triangle)
                   
bc(.5, .2)
#> [1] 0.5 0.2 0.3

bc(.5, c(.2, .1))
#> Error: bc(x = 0.5, y = c(0.2, 0.1))
#> Not scalar: `y`

bc(".5", 1)
#> Error: bc(x = ".5", y = 1)
#> 1) Not numeric: `x`
#> 2) (x, y) not in triangle
```

## Installation

Install from GitHub using the [devtools](https://github.com/hadley/devtools)
package:

```R
# install.packages("devtools")
devtools::install_github("egnha/valaddin")
```

See the package documentation `?strictly`, `help(p = valaddin)` for detailed 
information about `strictly()` and its companion functions.

## Related packages

* [assertthat](https://github.com/hadley/assertthat) provides a handy collection
of predicate functions that you can use with `strictly()`.

* [argufy](https://github.com/gaborcsardi/argufy) takes a different approach to
input validation, using [roxygen](https://github.com/klutometis/roxygen)
comments to specify checks.

* [ensurer](https://github.com/smbache/ensurer) provides a means of validating
function values, along with a replacement for `function()` to build functions
with type-validated arguments.

* [typeCheck](https://github.com/jimhester/typeCheck), together with [Types for
R](https://github.com/jimhester/types), enables the creation of functions with
type-validated arguments by means of special type annotations. This approach is
"dual" to that of valaddin: whereas valaddin specifies input checks as
_predicate functions with scope_, typeCheck specifies input checks as _arguments
with type_.

## License

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)
