
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actuary

<!-- badges: start -->

<!-- badges: end -->

The goal of actuary package is to support the actuarial workload.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zchmielewska/actuary")
```

## Example

This is an example of loading EIOPA RFR curves:

``` r
library(actuary)
load_RFR("20181231", "Euro")
load_RFR("20190331", "Poland", VA = TRUE)
load_RFR("20190630", "United States", scenario = "up")
```

Load VA and other parameters:

``` r
library(actuary)
load_VA("20181231", "Poland")
load_parameter("20181231", country = "Bulgaria", parameter = "UFR")
```
