# canPlotR 0.0.1
* Added a `NEWS.md` file to track changes to the package.
* Generate and export a rudimentary plot

## Known Issues
1. Regression does not reset to "none" when changing from numeric x-variable to categorical x-variable, leading to error
2. Optimization needed relating to `tryCatch` functions
  + eventually replace with `shiny::isTruthy` for application functions

## Planned Features
[ ] Re-organize UI to be more user-friendly
[ ] Remove all cases of error messages and replace with more informative messages, such as through shiny pop-ups
[ ] Generate datasets to test all transformations (date, time-POSIX, logit, probit, exp), numeric and multiple categorical data, as well as very large datasets to test limits
[ ] Generate summary data through reactive expressions to be plotted, e.g. with geom_bar()
  + summary: mean, media, geomean
  + error: SD, SEM, 95% CI, coefficient of variation, median absolute deviation (MAD)
