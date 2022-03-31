#' geomean 
#'
#' @description Calculate the geometric mean of a numeric vector
#'
#' @return Returns the geometric mean of the vector
#'
#' @noRd

geomean <- function(x, na.rm = T) {
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}