#' Dose-response of cell lines to investigative compound treatment
#'
#' A dataset containing the viability response of two cell lines to
#' different concentrations of series of investigative compounds. There
#' are two technical replicates for each compound/concentration combination.
#'
#' @format A tibble containing 240 rows and 4 variables:
#' \describe{
#'   \item{Cell}{Name of the cell line being treated}
#'   \item{Compound}{Name of the compound used}
#'   \item{Concentration (nM)}{The nanomolar (nM) concentration of the compound}
#'   \item{Viability}{The fractional effect of treatment on the measured viability of cells}
#'   ...
#' }
#' @source Author's dataset.
"example_dr"

#' Weight versus age of chicks on different diets
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A tibble containing 578 rows and 4 variables:
#' \describe{
#'   \item{weight}{a numeric vector giving the body weight of the chick (gm).}
#'   \item{Time}{a numeric vector giving the number of days since birth when the measurement was made.}
#'   \item{Chick}{an ordered factor with levels 18 < ... < 48 giving a unique identifier for the chick. The ordering of the levels groups chicks on the same diet together and orders them according to their final weight (lightest to heaviest) within diet.}
#'   \item{Diet}{a factor with levels 1, ..., 4 indicating which experimental diet the chick received.}
#'   ...
#' }
#' @source Crowder, M. and Hand, D. (1990), Analysis of Repeated Measures, Chapman and Hall (example 5.3)
#' Hand, D. and Crowder, M. (1996), Practical Longitudinal Data Analysis, Chapman and Hall (table A.2)
#' Pinheiro, J. C. and Bates, D. M. (2000) Mixed-effects Models in S and S-PLUS, Springer.
"example_ChickWeight"
