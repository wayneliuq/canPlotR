#' transformContinuous 
#'
#' @description Function which returns the types of numeric transformations
#' available to the user for continuous variables, which are supported by
#' ggplot2.
#'
#' @return A list of transformations supported by ggplot2.
#'
#' @noRd

transformContinuous <- function() {
  c(
    "none" = "identity",
    "reverse",
    "log10",
    "log2",
    "natural log" = "log",
    "sqrt",
    "exp",
    "logit",
    "probit",
    "date",
    "time hms" = "hms",
    "time POSIX" = "time"
  )
}