#### Helper functions ####

#' t-Distribution defined in terms of mean, scale, and df
#' @param x Quantile
#' @param mu Mean
#' @param r Scale parameter
#' @param kappa Degrees of freedom
#' @param log logical; if TRUE, densities are given as log(p)

dtss <- function(x, mu, r, kappa, log = FALSE) {

  out <- - log(r) + lgamma((kappa + 1)/2) - .5*(log(pi) + log(kappa)) -
    lgamma(kappa/2) - (kappa + 1)/2 * log(1 + ((x - mu)/r)^2/kappa)

  if ( ! log)
    out <- exp(out)

  return(out)

}
