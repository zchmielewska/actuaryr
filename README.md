
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actuary

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/zchmielewska/actuaryr.svg?branch=master)](https://travis-ci.org/zchmielewska/actuaryr)
<!-- badges: end -->

The goal of actuary package is to support the actuarial workload.

## Example

Retrieve the date in reference to the base date.

``` r
library(actuaryr)
```

Retrieve:

  - the first day of a month
  - the first day of a quarter
  - the first day of an year

<!-- end list -->

``` r
dref_fdom("2019-09-21")
#> [1] "2019-09-01"
dref_fdoq("2019-09-21")
#> [1] "2019-07-01"
dref_fdoy("2019-09-21")
#> [1] "2019-01-01"
```

Retrieve:

  - the first working day of a month
  - the first working day of a quarter
  - the first working day of an year

<!-- end list -->

``` r
dref_fwdom("2019-09-21")
#> [1] "2019-09-02"
dref_fwdoq("2019-09-21")
#> [1] "2019-07-01"
dref_fwdoy("2019-09-21")
#> [1] "2019-01-01"
```

Retrieve:

  - the last day of a month
  - the last day of a quarter
  - the last day of an year

<!-- end list -->

``` r
dref_ldom("2019-09-21")
#> [1] "2019-09-30"
dref_ldoq("2019-09-21")
#> [1] "2019-09-30"
dref_ldoy("2019-09-21")
#> [1] "2019-12-31"
```

Retrieve:

  - the last working day of a month
  - the last working day of a quarter
  - the last working day of an year

<!-- end list -->

``` r
dref_lwdom("2019-09-21")
#> [1] "2019-09-30"
dref_lwdoq("2019-09-21")
#> [1] "2019-09-30"
dref_lwdoy("2019-09-21")
#> [1] "2019-12-31"
```
