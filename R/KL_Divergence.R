#### Kullback-Leibler Divergence Measures Between Two Posteriors ####

#' Kullback-Leibler Divergence for the posteriors in the correlation test
#' @param N Sample size
#' @param R Correlation in the sample
#' @param alpha_prior Alpha parameter of the prior
#' @param beta_prior Beta parameter of the prior

KLdiv_cor <- function(N, R, alpha_prior, beta_prior){

  f1 <- function(rho) {
    res <- cor_posterior(rho, N = N, R = R, alpha_prior = alpha_prior, beta_prior = beta_prior) #informed
    res <- ifelse(res==0, 1e-298, res)
    res
  }
  f2 <- function(rho) {
    res <- cor_posterior(rho, N = N, R = R, alpha_prior = 1, beta_prior = 1) #default
    res <- ifelse(res==0, 1e-298, res)
    res
  }

  integrand <- function(rho) {
    f1r <- f1(rho)
    f1r*log(f1r/f2(rho))
  }

  integrate(integrand, lower=0, upper=1)$value
}

#' Kullback-Leibler Divergence for the posteriors in the t-test with normal prior
#' @param t Sample t-value
#' @param n1 Sample size group1
#' @param n2 Sample size group2
#' @param independentSamples Is it an independent samples t-test?
#' @param direction What is the direction of the t-test? greater / less / two.sided
#' @param prior.mean Mean of the prior distribution
#' @param prior.variance Variance of the prior distribution

KLdiv_tTest_norm <- function(t, n1, n2, independentSamples, direction, prior.mean, prior.variance){

  a <- ifelse(direction=="greater", 0, -Inf)
  b <- ifelse(direction=="less", 0, Inf)

  f1 <- function(delta) {
    posterior_normal_trunc(delta, a=a, b=b, t, n1, n2, independentSamples,
                            prior.mean = prior.mean, prior.variance = prior.variance)
  }

  f2 <- function(delta){
    posterior_t_trunc(delta, a=a, b=b, t, n1, n2, independentSamples, prior.location = 0,
                       prior.scale = sqrt(2)/2, prior.df = 1)
  }

  integrand <- function(delta) {
    f1d <- f1(delta)
    f2d <- f2(delta)
    if(any(is.na(c(f1d,f2d)))) return(NA)
    f1d*log(f1d/f2d)
  }

  if(is.na(integrand(1))) return(NA)

  tryCatch(integrate(integrand, lower=a, upper=b)$value,
           error=function(e) NA)
}

#' Kullback-Leibler Divergence for the posteriors in the t-test with t-prior
#' @param t Sample t-value
#' @param n1 Sample size group1
#' @param n2 Sample size group2
#' @param independentSamples Is it an independent samples t-test?
#' @param direction What is the direction of the t-test? greater / less / two.sided
#' @param prior.location Location parameter in the prior distribution
#' @param prior.scale Scale parameter in the prior distribution
#' @param prior.df Degrees of freedom of the prior distribution

KLdiv_tTest_t <- function(t, n1, n2, independentSamples, direction, prior.location, prior.scale, prior.df){

  a <- ifelse(direction=="greater", 0, -Inf)
  b <- ifelse(direction=="less", 0, Inf)

  f1 <- function(delta){
    posterior_t_trunc(delta, a=a, b=b, t, n1, n2, independentSamples,
                       prior.location = prior.location,
                       prior.scale = prior.scale,
                       prior.df = prior.df)
  }

  f2 <- function(delta){
    posterior_t_trunc(delta, a=a, b=b, t, n1, n2, independentSamples,
                       prior.location = 0, prior.scale = sqrt(2)/2,
                       prior.df = 1)
  }

  integrand <- function(delta) {
    f1d <- f1(delta)
    f2d <- f2(delta)
    if(any(is.na(c(f1d,f2d)))) return(NA)
    f1d*log(f1d/f2d)
  }

  if(is.na(integrand(1))) return(NA)

  tryCatch(integrate(integrand, lower=a, upper=b)$value,
           error=function(e) NA)
}


