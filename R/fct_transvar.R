#' transvar 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
transvar <- function(fct, x) {
  if (fct %in% c("log10", "log2", "log", "sqrt", "exp")) {
    match.fun(fct)(x)
  } else if (fct == "logit") {
    qlogis(x)
  } else if (fct == "probit") {
    qnorm(x)
  } else {
    x
  }
}

inversetrans <- function(fct, x) {
  if (fct == "log10") {
    10^x
  } else if (fct == "log2") {
    2^x
  } else if (fct == "log") {
    exp(x)
  } else if (fct == "sqrt") {
    x^2
  } else if (fct == "exp") {
    log(x)
  } else if (fct == "logit") {
    plogis(x)
  } else if (fct == "probit") {
    pnorm(x)
  } else {
    x
  }
}

seFind <- function(fit, se, fct, trans) {
  mapply(
    FUN = match.fun(fct),
    inversetrans(fct = trans, x = fit - se),
    inversetrans(fct = trans, x = fit + se)
  )
}