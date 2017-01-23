# Valaddin

*Valaddin* is a simple R package that provides a function `strictly()` that 
enables you to transform a function into a function with input validation 
checks.

A common form of boilerplate code at the top of functions is argument checking:
You make some checks on the arguments, signal a condition if any show-stopping
checks fail, then move on to the meat of the function if everything is good.
This approach, while straightforward, can clutter up the main work of a function
with admin; it spoils the fun in "function" with the inconvenience of a
security check.

This package provides a set of basic tools to add argument checks in an 
alternative functional manner, which is especially handy for interactive use.

## Installation

Install from GitHub using the [devtools](https://github.com/hadley/devtools)
package:

```R
# install.packages("devtools")
devtools::install_github("egnha/valaddin")
```

## Examples

The following simple example shows how to use `strictly()` to "harden" a 
function through the addition of input validation checks.

Consider the function `bc()` that computes the [barycentric 
coordinates](https://en.wikipedia.org/wiki/Barycentric_coordinate_system) of a 
point (x, y) in the plane (with respect to the triangle with vertices (0, 0), 
(0, 1), (1, 0)):

```R
bc <- function(x, y) c(x, y, 1 - x - y)
```

This function returns a triple for "good values" of `x` and `y`, but can yield 
unexpected or ambiguous results in other situations, for example when `x` is not
a scalar.

```R
bc(1, 2)
#> [1]  1  2 -2

bc(c(1, 2), 3)
#> [1]  1  2  3 -3 -4
```

Implicitly, the arguments of `bc()` should be scalars. We can make this 
assumption *explicit* by augmenting `bc()` with checks that verify that the
arguments are indeed numerical scalars. This can be done using `strictly()`:

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
The formula `~ is_number` succinctly articulates a global argument check, which 
asserts that each of `is_number(x)`, `is_number(y)` is `TRUE`. The transformed 
function `bc_num()` behaves exactly like `bc()`, only more "strictly" so.

Likewise, the implicit assumption that `x`, `y` are the coordinates of a point 
inside the triangle with vertices (0, 0), (0, 1), (1, 0) can be enforced by an 
additional check of the positivity of each of `x`, `y`, `1 - x - y`. (Following
the package [magrittr](https://github.com/tidyverse/magrittr), anonymous
functions of a `.` argument can be expressed via enclosure by curly braces
`{ ... }`.)

```R
barycentric_coord <- strictly(bc_num, list(~ x, ~ y, ~ 1 - x - y) ~ {. > 0})

barycentric_coord(.5, .2)
#> [1] 0.5 0.2 0.3

barycentric_coord(1, .2)
#> Error: barycentric_coord(x = 1, y = 0.2)
#> FALSE: (function(.) {. > 0})(1 - x - y)

barycentric_coord(.5, "2")
#> Error: barycentric_coord(x = 0.5, y = "2")
#> 1) FALSE: is.numeric(y)
#> 2) Error evaluating check (function(.) {. > 0})(1 - x - y): non-numeric argument to binary operator
```

Alternatively, input validation checks can be added in stages using the
[magrittr](https://github.com/tidyverse/magrittr) pipe operator, `%>%`.

```R
library(magrittr)

barycentric_coord <- bc %>%
  strictly(~ is_number, list(~ x, ~ y, ~ 1 - x - y) ~ {. > 0})
  
barycentric_coord <- bc %>%
  strictly(~ is_number) %>%
  strictly(list(~ x, ~ y, ~ 1 - x - y) ~ {. > 0})
  
barycentric_coord(.5, .2)
#> [1] 0.5 0.2 0.3

barycentric_coord(1, .2)
#> Error: barycentric_coord(x = 1, y = 0.2)
#> FALSE: (function(.) {. > 0})(1 - x - y)
```

For more information on the use of `strictly()`, and its companion functions,
see the package documentation `?valaddin::strictly`.

## Related packages

* The [argufy](https://github.com/gaborcsardi/argufy) package takes a different 
approach to input validation, using
[roxygen](https://github.com/klutometis/roxygen) comments to specify checks.

* The [ensurer](https://github.com/smbache/ensurer) package provides a means of 
validating function _values_, along with a replacement for `function()` to build
functions with type-validated arguments. valaddin complements ensurer.

* [Types for R](https://github.com/jimhester/types) is a package that provides 
type annotations for function arguments.

## License

MIT Copyright © 2017, [Eugene Ha](https://github.com/egnha)
