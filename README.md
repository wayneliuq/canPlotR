
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canPlotR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

canPlotR is a browser-based tool to generate and export presentation-
and publication-quality plots and figures, currently prioritizing
functions for scientific plots. canPlotR aims to use an intuitive user
interface to wrap `ggplot2` graphing functions to create professional
and highly customizable figures required for most use cases.

## Installation

You can install the development version of canPlotR from Github so:

    # If you don't have the `devtools` package installed, you can install it from CRAN with
    # install.packages("devtools")
    library(devtools)
    install_github("wayneliuq/canplotr")

## How to Use

### Preparing the stacked input file

Unlike most common applications for making plots (Excel, GraphPad),
canPlotR is designed to use the much more versatile *stacked* tables to
make plots.

**To be updated**

### Loading the app

To run canPlotR, load the package and run it:

    library(canPlotR)
    canPlotR::run_app()
