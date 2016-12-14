# Valaddin

A common form of boilerplate code at the top of functions is argument checking: You make some checks on the arguments, signal a condition if any show-stopping checks fail, then move on to the meat of the function if everything is good. The problem with this approach is that it can clutter up the main work of a function with admin; it spoils the fun in "function"" with the inconvenience of a security check.

This package provides a set of basic tools to add argument checks in a functional manner.