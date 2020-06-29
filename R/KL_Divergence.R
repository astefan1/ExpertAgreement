#### Kullback-Leibler Divergence Measures Between Two Posteriors ####

# Kullback-Leibler Divergence for the posteriors in the correlation test
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
    f1(rho)*(log(f1(rho))-log(f2(rho)))
  }

  integrate(integrand, lower=0, upper=1)$value
}

# Kullback-Leibler Divergence for the posteriors in the t-test with normal prior
KLdiv_tTest_norm <- function(t, n1, n2, independentSamples, prior.mean, prior.variance, direction){

  if(direction=="two-sided"){
    a <- -Inf
    b <- Inf
  } else if(direction=="larger"){
    a <- 0
    b <- Inf
  } else if(direction=="less"){
    a <- -Inf
    b <- 0
  }

  f1 <- function(delta) {
    res <- posterior_normal_trunc(delta, a=a, b=b, t, n1, n2, independentSamples,
                            prior.mean = prior.mean, prior.variance = prior.variance)
    res <- ifelse(res==0, 1e-298, res)
    res
  }

  f2 <- function(delta){
    res <- posterior_t_trunc(delta, a=a, b=b, t, n1, n2, independentSamples, prior.location = 0,
                       prior.scale = sqrt(2)/2, prior.df = 1)
    res <- ifelse(res==0, 1e-298, res)
    res
  }

  integrand <- function(delta) {
    f1(delta)*(log(f1(delta))-log(f2(delta)))
  }

  integrate(integrand, lower=0, upper=Inf, subdivisions = 2000)$value
}

# Kullback-Leibler Divergence for the posteriors in the t-test with t-prior
KLdiv_tTest_t <- function(t, n1, n2, independentSamples, prior.location, prior.scale, prior.df, direction){

  if(direction=="two-sided"){
    a <- -Inf
    b <- Inf
  } else if(direction=="larger"){
    a <- 0
    b <- Inf
  } else if(direction=="less"){
    a <- -Inf
    b <- 0
  }

  f1 <- function(delta){
    res <- posterior_t_trunc(delta, a=a, b=b, t, n1, n2, independentSamples,
                       prior.location = prior.location,
                       prior.scale = prior.scale,
                       prior.df = prior.df)
    res <- ifelse(res==0, 1e-298, res)
    res
  }

  f2 <- function(delta){
    res <- posterior_t_trunc(delta, a=a, b=b, t, n1, n2, independentSamples,
                       prior.location = 0, prior.scale = sqrt(2)/2,
                       prior.df = 1)
    res <- ifelse(res==0, 1e-298, res)
    res
  }

  integrand <- function(delta) {
    f1(delta)*(log(f1(delta))-log(f2(delta)))
  }

  integrate(integrand, lower=0, upper=Inf)$value
}


