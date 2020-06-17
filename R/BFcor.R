#### Code for correlation Bayes factors and posteriors with informed priors ####

# Based on Ly, Maarsman & Wagenmakers (2018). doi:10.1111/stan.12111

# Reduced likelihood

#' @import hypergeo
#' @import logOfGamma

A_nr <- function(N, R, Rho, Gamma, Delta){
  A <- (N-Gamma-1)/2
  B <- (N-Delta-1)/2
  Z <- R^2*Rho^2
  Hyper <- hypergeo::hypergeo(A, B, 0.5, Z, tol = .Machine$double.eps^0.25)
  overflow100 <- hypergeo::hypergeo((99-Gamma)/2, (99-Delta)/2, 0.5, Z)/hypergeo::hypergeo((100-Gamma)/2, (100-Delta)/2, 1.5, Z)
  overflow50 <- hypergeo::hypergeo((49-Gamma)/2, (49-Delta)/2, 0.5, Z)/hypergeo::hypergeo((50-Gamma)/2, (50-Delta)/2, 1.5, Z)
  overflow <- Re((overflow100-overflow50)/50)
  overflowIntrc <- Re(overflow100)-100*overflow
  Hyper[is.infinite(Hyper) & Re(Hyper) > 0] <- (N*overflow+overflowIntrc)*1e+298
  Rhoterm <- (1-Rho^2)^((N-Gamma-Delta-1)/2)
  Rhoterm[Rhoterm<1e-298] <- 1e-298
  Rhoterm*Hyper
}

W_n <- function(N, Gamma, Delta){
  term1 <- logOfGamma::gammaln((N-Gamma)/2) + logOfGamma::gammaln((N-Delta)/2)
  term2 <- logOfGamma::gammaln((N-Gamma-1)/2) + logOfGamma::gammaln((N-Delta-1)/2)
  exp(term1-term2)
}

B_nr <- function(N, R, Rho, Gamma, Delta){
  term1 <- 2*R*Rho*(1-Rho^2)^((N-Gamma-Delta-1)/2)
  term1[term1 == 0] <- 2*R*Rho*1e-298
  A <- (N-Gamma)/2
  B <- (N-Delta)/2
  Z <- R^2*Rho^2
  Hyper <- hypergeo::hypergeo(A, B, 1.5, Z)
  Hyper[Re(Hyper) > 1e+298] <- 1e+298
  res <- term1 * (W_n(N, Gamma, Delta)) * Hyper
  return(res)
}

L_red <- function(N, R, Rho, Gamma, Delta){
  A_nr(N, R, Rho, Gamma, Delta) + B_nr(N, R, Rho, Gamma, Delta)
}

# Unstandardized posterior

cor_posterior_US <- function(Rho, N, R, alpha_prior, beta_prior){
  res <- log(L_red(N, R, Rho, Gamma = 0, Delta = 0)) + stats::dbeta(Rho, alpha_prior, beta_prior, log=TRUE)
  return(exp(Re(res)))
}

# Marginal likelihood

cor_ML <- function(N, R, alpha_prior, beta_prior){
  stats::integrate(function(x) cor_posterior_US(x, N, R, alpha_prior, beta_prior),
            lower = 0, upper = 1, subdivisions = 10000, abs.tol = 1e-3, stop.on.error = F)$value

}

#' Standardized posterior for Pearson correlation with informed prior
#' @param Rho Correlation parameter
#' @param N Sample size
#' @param R Pearson correlation coefficient in the sample
#' @param alpha_prior Alpha parameter in the beta prior on r
#' @param beta_prior Beta parameter in the beta prior on r
#' @export

cor_posterior <- function(Rho, N, R, alpha_prior, beta_prior){
  enum <- cor_posterior_US(Rho, N, R, alpha_prior, beta_prior)
  denom <- cor_ML(N, R, alpha_prior, beta_prior)
  enum/denom
}

#' Bayes factor for Pearson correlation with informed prior
#' @param N Sample size
#' @param R Pearson correlation coefficient in the sample
#' @param alpha_prior Alpha parameter in the beta prior on r
#' @param beta_prior Beta parameter in the beta prior on r
#' @importFrom stats integrate dbeta
#' @export

cor_BF10 <- function(N, R, alpha_prior, beta_prior){
  L0 <- Re(L_red(N, R, Rho=0, Gamma=0, Delta=0))
  L1 <- cor_ML(N, R, alpha_prior, beta_prior)
  L1/L0
}
