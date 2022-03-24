# Known Issues
1. There issues with the following continuous transformations don't work. Generate artificial datasets to test all of these transformations: 
  + "exp" (missing value where TRUE/FALSE needed) works for Viability
  + "logit" ('from' must be a finite number) works for Viability
  + "probit" ('from' must be a finite number) works for Viability
  + "date" and "time POSIX" - untested
2. Before the first time the tab for column selection is loaded, trying to click "update plot" gives an error.

# Planned Features
[] Remove all cases of error messages and replace with more informative messages
  + numeric scale transformation errors
[] Stratify data by colour, shape, size, and facets
[] Change plot output dimensions and resolution
[] Ability to export pdf figure in the specified resolution

# canPlotR 0.0.1
* Added a `NEWS.md` file to track changes to the package.