The main purpose of this release is to repair breakages introduced by the recent
release of purrr 0.2.3. Other bugs fixes, and some minor features additions, 
were also made (see NEWS.md).

## R test environments

* OS X 10.11.6:
  - 3.4.1
  
* Ubuntu 12.04.5 LTS (on Travis CI):
  - 3.3.3
  - 3.4.1
  - devel (2017-08-10 r73083)
  
* Windows (on win-builder):
  - 3.3.3
  - 3.4.1
  - devel (2017-08-09 r73082)

## R CMD check results

There were no ERRORs, WARNINGSs, or NOTESs for all test environments, with one
exception:

* for R 3.3.3 on x86_64-w64-mingw32 (64-bit), there was one NOTE:

    > checking CRAN incoming feasibility ... NOTE
    > Maintainer: 'Eugene Ha <eha@posteo.de>'
    > 
    > License components with restrictions and base license permitting such:
    >   MIT + file LICENSE
    > File 'LICENSE':
    >   YEAR: 2017
    >   COPYRIGHT HOLDER: Eugene Ha

## Downstream dependencies

None found with `tools::package_dependencies()`.
