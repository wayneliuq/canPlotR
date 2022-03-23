
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canPlotR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

canPlotR is a browser-based tool to generate and export presentation-
and publication-ready plots and figures. canPlotR aims to use an
intuitive user interface to wrap `ggplot2` graphing functions to create
professional and highly customizable figures required for most use
cases.

## Installation

You can install the development version of canPlotR from Github so:

``` r
# If you don't have the `devtools` package installed, you can install it from CRAN with
# install.packages("devtools")
library(devtools)
install_github("wayneliuq/canplotr")
```

## How to Use

### Preparing the stacked input file

Unlike most common applications for making plots (Excel, GraphPad),
canPlotR is designed to use the much more versatile *stacked* tables to
make plots.

**To be updated**

### Loading the app

To run canPlotR, load the package and run it:

``` r
library(canPlotR)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: ggplot2
canPlotR::run_app()
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>
