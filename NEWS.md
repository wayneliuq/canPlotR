# canPlotR 0.0.1

* Added a `NEWS.md` file to track changes to the package.

# Current Issues
1. There issues with the following continuous transformations don't work: 

  + "exp" (missing value where TRUE/FALSE needed) works for Viability
  + "logit" ('from' must be a finite number) works for Viability
  + "probit" ('from' must be a finite number) works for Viability
  + "date" amd "time POSIX" - untested

# Planned Features
1. Remove all cases of error messages and replace with more informative messages
  + numeric scale transformation errors
2. Stratify data by colour, shape, size, and facets
3. Change plot output dimensions and resolution
4. Export figure