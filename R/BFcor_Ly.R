#### Code for correlation Bayes factors and posteriors with informed priors ####
# Based on Ly, Maarsman & Wagenmakers (2018). doi:10.1111/stan.12111
# Allows to set Gamma and Delta parameters freely (they are fixed to zero in
# the regular functions)

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

cor_posterior_US_Ly <- function(Rho, N, R, alpha_prior, beta_prior, Gamma, Delta){
  res <- log(L_red(N, R, Rho, Gamma = Gamma, Delta = Delta)) + stats::dbeta(Rho, alpha_prior, beta_prior, log=TRUE)
  return(exp(Re(res)))
}

# Marginal likelihood

cor_ML_Ly <- function(N, R, alpha_prior, beta_prior, Gamma, Delta){
  stats::integrate(function(x) cor_posterior_US_Ly(x, N, R, alpha_prior, beta_prior, Gamma, Delta),
            lower = 0, upper = 1, subdivisions = 10000, abs.tol = 1e-3, stop.on.error = F)$value

}

#' Standardized posterior for Pearson correlation with informed prior
#' @param Rho Correlation parameter
#' @param N Sample size
#' @param R Pearson correlation coefficient in the sample
#' @param alpha_prior Alpha parameter in the beta prior on r
#' @param beta_prior Beta parameter in the beta prior on r
#' @param Gamma Gamma parameter (see Ly et al. 2018)
#' @param Delta Delta parameter (see Ly et al. 2018)
#' @export

cor_posterior_Ly <- function(Rho, N, R, alpha_prior, beta_prior, Gamma, Delta){
  enum <- cor_posterior_US_Ly(Rho, N, R, alpha_prior, beta_prior, Gamma, Delta)
  denom <- cor_ML_Ly(N, R, alpha_prior, beta_prior, Gamma, Delta)
  enum/denom
}

#' Bayes factor for Pearson correlation with informed prior
#' @param N Sample size
#' @param R Pearson correlation coefficient in the sample
#' @param alpha_prior Alpha parameter in the beta prior on r
#' @param beta_prior Beta parameter in the beta prior on r
#' @param Gamma Gamma parameter (see Ly et al. 2018)
#' @param Delta Delta parameter (see Ly et al. 2018)
#' @importFrom stats integrate dbeta
#' @export

cor_BF10_Ly <- function(N, R, alpha_prior, beta_prior, Gamma=0, Delta=0){
  L0 <- Re(L_red(N, R, Rho=0, Gamma=Gamma, Delta=Delta))
  L1 <- cor_ML_Ly(N, R, alpha_prior, beta_prior, Gamma, Delta)
  suppressWarnings(L1/L0)
}
