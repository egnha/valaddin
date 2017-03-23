## Resubmission (23-03-2017)

This is a resubmission to fix the following issue:

> Found the following (possibly) invalid URLs:
>  URL: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
>    From: inst/doc/valaddin.html
>    Status: 200
>    Message: OK
>    CRAN URL not in canonical form
>  The canonical URL of the CRAN page for a package is 
>    https://CRAN.R-project.org/package=pkgname

As requested, I have normalized the problematic link to

https://cran.r-project.org/package=lazyeval/vignettes/lazyeval.html

No other changes were made.

## Test environments

* OS X El Capitan 10.11.6: R 3.3.2
* Ubuntu 12.04.5 LTS (on Travis CI): R 3.2.5, 3.3.2, devel (2017-03-22 r72384)
* Windows (on win-builder): R 3.3.3, devel (2017-03-21 r72375)

## R CMD check results

There were no ERRORs or WARNINGSs.

There was one NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Eugene Ha <eha@posteo.de>’
  
  New submission
  
  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    YEAR: 2017
    COPYRIGHT HOLDER: Eugene Ha

## Downstream dependencies

None. (This is the first submission.)
