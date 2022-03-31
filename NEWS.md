# Known Issues
1. There issues with the following continuous transformations don't work. 
  + "exp" (missing value where TRUE/FALSE needed) works for Viability
  + "logit" ('from' must be a finite number) works for Viability
  + "probit" ('from' must be a finite number) works for Viability
  + "date" and "time POSIX" - untested
2. Reactable display area shows an error before any data is loaded
3. Optimization needed relating to `tryCatch` functions

# Planned Features
[ ] Remove all cases of error messages and replace with more informative messages
  + numeric scale transformation errors
[ ] Stratify data by colour, shape, size, and facets
[ ] Change plot output dimensions and resolution
[ ] Ability to export pdf figure in the specified resolution
[ ] Generate datasets to test all transformations (date, time-POSIX, logit, probit, exp), numeric and multiple categorical data, as well as very large datasets to test limits
[ ] Generate summary data through reactive expressions to be plotted, e.g. with geom_bar()
  + summary: mean, media, geomean
  + error: SD, SEM, 95% CI, coefficient of variation, median absolute deviation (MAD)

# canPlotR 0.0.1
* Added a `NEWS.md` file to track changes to the package.