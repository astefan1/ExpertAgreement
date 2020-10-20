#### Code for correlation Bayes factors and posteriors with informed priors ####

#'@import SuppDists
# Unstandardized posterior distribution
cor_posterior_US <- function(rho, N, R, alpha_prior, beta_prior){
  SuppDists::dPearson(R, N, rho=rho)*stats::dbeta(rho,alpha_prior, beta_prior)
}

# Marginal likelihood
cor_ML <- function(N, R, alpha_prior, beta_prior){
  stats::integrate(cor_posterior_US, N = N, alpha_prior=alpha_prior,
                   beta_prior=beta_prior, R = R, lower = 0, upper = 1,
                   subdivisions = 2000)$value

}

# Posterior distribution
cor_posterior <- function(rho, N, R, alpha_prior, beta_prior){
  enum <- cor_posterior_US(rho, N, R, alpha_prior, beta_prior)
  denom <- cor_ML(N, R, alpha_prior, beta_prior)
  enum/denom
}

# Bayes factor
cor_BF10 <- function(N, R, alpha_prior, beta_prior){
  L0 <- dPearson(R, N, rho=0)
  L1 <- cor_ML(N, R, alpha_prior, beta_prior)
  suppressWarnings(L1/L0)
}

# a=Sys.time()
# cor_BF10(100, 0.3, 3, 3)
# Sys.time()-a
