# valaddin

## 0.1.0

* First stable release:
    + Main functional operators: `firmly`, `loosely`
    + Component extractors: `firm_checks`, `firm_core`, `firm_args`
    + Check-scope converters (checker factories): `localize`, `globalize`
    + Localized base-R- and purrr-predicate checkers: `vld_*`
    
* `vld_numeric`, `vld_scalar_numeric` are based on `base::is.numeric`, since the
corresponding predicates in purrr will be deprecated starting from version 
0.2.2.9000 ([#12](https://github.com/egnha/valaddin/issues/12))

* Fulfills aim of purrr proposal 
[#275](https://github.com/hadley/purrr/issues/275) (closed)
