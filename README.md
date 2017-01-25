# Valaddin

[![Travis-CI Build Status](https://travis-ci.org/egnha/valaddin.svg?branch=master)](https://travis-ci.org/egnha/valaddin)
[![codecov](https://codecov.io/gh/egnha/valaddin/branch/master/graph/badge.svg)](https://codecov.io/gh/egnha/valaddin)

Dealing with invalid function inputs is a pervasive problem in R, given R's 
weakly typed nature. _Valaddin_ is a simple R package that provides a function
`strictly()` that enables you to enhance a function with input validation
checks, in a manner suitable for both programmatic use and interactive sessions.

## Installation

Install from GitHub using the [devtools](https://github.com/hadley/devtools)
package:

```R
# install.packages("devtools")
devtools::install_github("egnha/valaddin")
```

## Examples

The following example illustrates the use of `strictly()` to "harden" a function
through the (successive) addition of input validation checks.

Consider the following function `bc()`, which computes the [barycentric 
coordinates](https://en.wikipedia.org/wiki/Barycentric_coordinate_system) of a 
point (x, y) in the plane, with respect to the triangle with vertices (0, 0), 
(0, 1), (1, 0):

```R
bc <- function(x, y) c(x, y, 1 - x - y)
```

This function returns a triple for "good values" of `x` and `y`, but can yield 
unexpected or ambiguous results when `x` is not a scalar.

```R
bc(1, 2)
#> [1]  1  2 -2

bc(c(1, 2), 3)
#> [1]  1  2  3 -3 -4
```

The arguments of `bc()` are _assumed_ to be scalars. We can make this assumption
explicit by augmenting `bc()` with checks that verify that the arguments are 
indeed numerical scalars. This is where `strictly()` comes in.

```R
library(valaddin)

is_number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
bc_num <- strictly(bc, ~ is_number)

bc_num(1, 2)
#> [1]  1  2 -2

bc_num(c(1, 2), 3)
#> Error: bc_num(x = c(1, 2), y = 3)
#> FALSE: is_number(x)
```
The formula `~ is_number` expresses a global argument check—the assertion that
each of `is_number(x)`, `is_number(y)` is `TRUE`. The transformed function
`bc_num()` behaves exactly like `bc()`, only more strictly so.

Likewise, the implicit assumption that `x`, `y` are the coordinates of a point
inside the triangle with vertices (0, 0), (0, 1), (1, 0) can be enforced by an
additional check of the positivity of each of `x`, `y`, `1 - x - y`. Following
the [magrittr](https://github.com/tidyverse/magrittr) package, an anonymous
function of a single argument `.` can be written inside curly braces `{ }`.

```R
barycentric_coord <- strictly(bc_num, list(~ x, ~ y, ~ 1 - x - y) ~ {. >= 0})

barycentric_coord(.5, .2)
#> [1] 0.5 0.2 0.3

barycentric_coord(1, .2)
#> Error: barycentric_coord(x = 1, y = 0.2)
#> FALSE: (function(.) {. >= 0})(1 - x - y)

barycentric_coord(.5, "2")
#> Error: barycentric_coord(x = 0.5, y = "2")
#> 1) FALSE: is_number(y)
#> 2) Error evaluating check (function(.) {. >= 0})(1 - x - y): non-numeric argument to binary operator
```

Alternatively, input validation checks can be added in stages, on-the-fly, using
the [magrittr](https://github.com/tidyverse/magrittr) pipe operator `%>%`.

```R
library(magrittr)

barycentric_coord <- bc %>%
  strictly(~ is_number) %>%
  strictly(list(~ x, ~ y, ~ 1 - x - y) ~ {. >= 0})
  
barycentric_coord(.5, .2)
#> [1] 0.5 0.2 0.3

barycentric_coord(.5, .6)
#> Error: barycentric_coord(x = 1, y = 0.2)
#> FALSE: (function(.) {. >= 0})(1 - x - y)
```

The _purpose_ of the positivity check on `x`, `y`, `1 - x - y` is to determine 
whether the point (x, y) lies in the triangle. To express this more directly, we
can use a custom error message and a multi-argument checking function, 
facilitated by the `lift()` function from the 
[purrr](https://github.com/hadley/purrr) package.

```R
library(purrr)

in_triangle <- function(x, y) x >= 0 && y >= 0 && 1 - x - y >= 0
barycentric_coord <- bc %>%
  strictly("Not a number" ~ is_number,
           list("Point (x, y) not in triangle" ~ list(x, y)) ~ lift(in_triangle))

barycentric_coord(.5, .2)
#> [1] 0.5 0.2 0.3

barycentric_coord(.5, ". 2")
#> Error: barycentric_coord(x = 0.5, y = ".2")
#> 1) Not a number: `y`
#> 2) Point (x, y) not in triangle

barycentric_coord(.5, .6)
#> Error: barycentric_coord(x = 0.5, y = 0.6)
#> Point (x, y) not in triangle
```

It is safe to reassign a function to its "strictification," because the 
underlying function is recoverable with `nonstrictly()`.

```R
nonstrictly(barycentric_coord)
#> function(x, y) c(x, y, 1 - x - y)

identical(bc, nonstrictly(barycentric_coord))
#> [1] TRUE
```

The package documentation `?valaddin::strictly`, `help(p = valaddin)` has more
information on the use of `strictly()`, and its companion functions.

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
This approach is "dual" to that of Valaddin: whereas Valaddin specifies input
checks as _predicate functions with scope_, typeCheck specifies input checks as
_arguments with type_.

## License

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)
