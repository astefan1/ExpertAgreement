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

dtss_trunc <- function(x, mu, r, kappa, a, b){

  fval <- dtss(x, mu, r, kappa)

  lower <- integrate(dtss, mu=mu, r=r, kappa=kappa, lower=-Inf, upper=a)$value
  upper <- integrate(dtss, mu=mu, r=r, kappa=kappa, lower=-Inf, upper=b)$value

  if(a==-Inf) lower <- 0
  if(upper-lower==0) return(0)
  fval/(upper-lower)

}

#'@importFrom stats optim

qtss_trunc <- function(x, mu, r, kappa, a, b){

  fn <- function(par){
    (integrate(dtss_trunc, lower=a, upper=par, mu=mu, r=r, kappa=kappa, a=a, b=b)$value-x)^2
  }

  # get reasonable starting value
  xval <- seq(max(-4, a),min(4, b),length.out = 100)
  start <- NA
  for(i in seq_along(xval)){
    start[i] <- fn(xval[i])
  }
  start <- min(start)

  round(stats::optim(par=start, fn=fn, method="Brent", lower=max(-10, a), upper=min(10, b))$par, 3)
}
