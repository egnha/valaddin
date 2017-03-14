# valaddin 0.0.0.9000

* `vld_numeric`, `vld_scalar_numeric` are based on `base::is.numeric`, since the corresponding predicates in purrr will be deprecated starting from version 0.2.2.9000 (#12)

* Initial feature complete version
    * Main functional operators: `firmly()`, `loosely()`
    * Component extractors: `firm_checks()`, `firm_core()`, `firm_args()`
    * Check-scope converters (checker factories): `localize()`, `globalize()`
    * Localized base- and purrr-predicate checkers: `vld_*()` (e.g., `vld_data_frame()`, `vld_bare_atomic()`)

* Fulfills aim of purrr proposal
[#275](https://github.com/hadley/purrr/issues/275) (closed)
