# valaddin

[![Travis-CI Build Status](https://travis-ci.org/egnha/valaddin.svg?branch=master)](https://travis-ci.org/egnha/valaddin)
[![codecov](https://codecov.io/gh/egnha/valaddin/branch/master/graph/badge.svg)](https://codecov.io/gh/egnha/valaddin)

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

If you want to make sure that `secant()` only accepts numerical inputs for `x`
and `dx`, you'd normally rewrite `secant()` to check this condition, and stop
if it's violated:

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

### But the standard approach is problematic

While this works, it's not ideal, even in this simple situation, because

* you have to declare a new function, and give it a new name, or copy-paste the
original function body

* it doesn't catch all errors, only the first that occurs among the checks.

Moreover, if you later realize you need additional checks, for example, that `x`
and `dx` should be numbers not vectors, you're back to square one:

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

### valaddin overcomes these problems

valaddin provides a function `strictly()` that takes care of input validation by
*transforming* the existing function, instead of forcing you to write a new one.
It also helps you by reporting *every* failing check.

```R
library(valaddin)

secant <- strictly(secant, list(~x, ~dx) ~ is.numeric)

secant(log, 1, .1)
#> [1] 0.9531018

secant(log, "1", ".1")
#> Error: secant(f = log, x = "1", dx = ".1")
#> 1) FALSE: is.numeric(x)
#> 2) FALSE: isi.numeric(dx)
```

And to add additional checks, just apply the same procedure again:

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

secant(log, "1", c(.1, .01))
#> Error: secant(f = log, x = "1", dx = c(0.1, 0.01))
#> 1) FALSE: (function(.) {is.numeric(.) && length(.) == 1L})(x)
#> 2) FALSE: (function(.) {is.numeric(.) && length(.) == 1L})(dx)
```

## Installation

Install from GitHub using the [devtools](https://github.com/hadley/devtools)
package:

```R
# install.packages("devtools")
devtools::install_github("egnha/valaddin")
```

See the package documentation `?strictly`, `help(p = valaddin)` for detailed information about `strictly()` and its companion functions.

## Related packages

* The [argufy](https://github.com/gaborcsardi/argufy) package takes a different 
approach to input validation, using
[roxygen](https://github.com/klutometis/roxygen) comments to specify checks.

* The [ensurer](https://github.com/smbache/ensurer) package provides a means of 
validating function values, along with a replacement for `function()` to build 
functions with type-validated arguments.

* The [typeCheck](https://github.com/jimhester/typeCheck) package, together with
[Types for R](https://github.com/jimhester/types), enables the creation of 
functions with type-validated arguments by means of special type annotations.
This approach is "dual" to that of valaddin: whereas valaddin specifies input
checks as _predicate functions with scope_, typeCheck specifies input checks as
_arguments with type_.

* The [assertthat](https://github.com/hadley/assertthat) package provides a 
handy collection of predicate functions that you can use with `strictly()`.

## License

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)
