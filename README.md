# dployr

<!-- badges: start -->
[![R-CMD-check](https://github.com/thomasp85/dployr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomasp85/dployr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/thomasp85/dployr/graph/badge.svg)](https://app.codecov.io/gh/thomasp85/dployr)
<!-- badges: end -->

dployr is a companion app to [producethis](https://github.com/r-lib/producethis) and takes care of all the runtime execution of a data science project.

## Installation

You can install the development version of dployr from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thomasp85/dployr")
```

## Example

dployr is generally not meant to be used interactively. Its main function is `execute()` which will be called in the `main.R` file in the root of a project
