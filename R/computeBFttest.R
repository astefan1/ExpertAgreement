#### Compute t-test Bayes factors for all datasets in the Wetzels dataset ####

# Prepare bf10_t function so that it runs on Wetzels and prior data
BF_Wetzels<- function(dat, prior){
  dat <- suppressWarnings(as.numeric(dat))
  prior <- suppressWarnings(as.numeric(prior))
  BFMATCHt <- suppressWarnings(bf10_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                      prior.location=prior[25], prior.scale=prior[26], prior.df=prior[27]))
  BFMATCHnorm <- suppressWarnings(bf10_normal(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                              prior.mean=prior[23], prior.variance=prior[24]^2))
  BFShinyt <- suppressWarnings(bf10_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                      prior.location=prior[31], prior.scale=prior[32], prior.df=prior[33]))
  BFShinynorm <- suppressWarnings(bf10_normal(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                              prior.mean=prior[29], prior.variance=prior[30]^2))
  res <- c(BFMATCHt, BFMATCHnorm, BFShinyt, BFShinynorm)
  names(res) <- paste0(c(rep("BF_MATCH_t_", 3), rep("BF_MATCH_norm_", 3),
                         rep("BF_Shiny_t_", 3), rep("BF_Shiny_norm_", 3)),
                       names(res))
  return(res)
}

# Compute BFs for every dataset in WetzelsEtAl
computeBFWetzels <- function(WetzelsEtAl, ExpertsPriors_tTest){
  res <- matrix(NA, nrow = 0, ncol = nrow(WetzelsEtAl))
  # Informed BFs for each expert
  for(i in 1:nrow(ExpertsPriors_tTest)){
    print(i)
    tmp <- simplify2array(apply(WetzelsEtAl,
                                MARGIN = 1,
                                function(x) BF_Wetzels(x, ExpertsPriors_tTest[i,])))
    rownames(tmp) <- paste0(rownames(tmp), "_Exp", i)
    res <- rbind(res, tmp)
  }
  # Default BFs
  BFdefault <- function(dat){
    dat <- suppressWarnings(as.numeric(dat))
    suppressWarnings(bf10_t(t=dat[3], n1=dat[1], n2=dat[2],
                            independentSamples=as.logical(dat[6]),
                            prior.location=0, prior.scale=sqrt(2)/2, prior.df=1))
  }
  tmp <- simplify2array(apply(WetzelsEtAl,
                              MARGIN = 1,
                              function(x) BFdefault(x)))
  rownames(tmp) <- paste0(rownames(tmp), "_default")
  res <- rbind(res, tmp)
  res.t <- t(res)

  return(res.t)
}

# Find BF that are zero due to numerical issues and check if they can be replaced
# by BF functions from BFDA package.

# Background: Usually, BF functions in the
# BFttest script and BF functions from BFDA package yield the same results.
# However, the BF functions in the BFDA package differ slightly in computational
# details from the BF functions used in the BFttest script, which makes them
# more stable in situations where the BFttest functions run into numerical
# issues, but less stable elsewhere.

#'@import BFDA

BF_WetzelsBFDA<- function(dat, prior){
  dat <- suppressWarnings(as.numeric(dat))
  prior <- suppressWarnings(as.numeric(prior))
  BFMATCHt <- suppressWarnings(BFDA:::bf10_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                            prior.location=prior[25], prior.scale=prior[26], prior.df=prior[27]))
  BFMATCHnorm <- suppressWarnings(BFDA:::bf10_normal(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                                    prior.mean=prior[23], prior.variance=prior[24]))
  BFShinyt <- suppressWarnings(BFDA:::bf10_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                            prior.location=prior[31], prior.scale=prior[32], prior.df=prior[33]))
  BFShinynorm <- suppressWarnings(BFDA:::bf10_normal(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                                    prior.mean=prior[29], prior.variance=prior[30]))
  BFdefault <- suppressWarnings(BFDA:::bf10_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                       prior.location=0, prior.scale=sqrt(2)/2, prior.df=1))
  res <- c(BFMATCHt, BFMATCHnorm, BFShinyt, BFShinynorm)
  names(res) <- paste0(c(rep("BF_MATCH_t_", 3), rep("BF_MATCH_norm_", 3),
                         rep("BF_Shiny_t_", 3), rep("BF_Shiny_norm_", 3)),
                       names(res))
  return(res)
}

computeBFWetzelsBFDA <- function(WetzelsEtAl, ExpertsPriors_tTest){
  res <- matrix(NA, nrow = 0, ncol = nrow(WetzelsEtAl))
  for(i in 1:nrow(ExpertsPriors_tTest)){
    print(i)
    tmp <- simplify2array(apply(WetzelsEtAl,
                                MARGIN = 1,
                                function(x) BF_WetzelsBFDA(x, ExpertsPriors_tTest[i,])))
    rownames(tmp) <- paste0(rownames(tmp), "_Exp", i)
    res <- rbind(res, tmp)
  }
  BFdefault <- function(dat){
    dat <- suppressWarnings(as.numeric(dat))
    suppressWarnings(BFDA:::bf10_t(t=dat[3], n1=dat[1], n2=dat[2],
                            independentSamples=as.logical(dat[6]),
                            prior.location=0, prior.scale=sqrt(2)/2, prior.df=1))
  }
  tmp <- simplify2array(apply(WetzelsEtAl,
                              MARGIN = 1,
                              function(x) BFdefault(x)))
  rownames(tmp) <- paste0(rownames(tmp), "_default")
  res <- rbind(res, tmp)
  res.t <- t(res)

  return(res.t)
}

recompute_BF_if_numeric_problems <- function(BFWetzels, WetzelsEtAl, ExpertsPriors_tTest){
  nulls <- which(BFWetzels== 0)
  if(length(nulls)!=0){
    rws <- unique(nulls-floor(nulls/nrow(WetzelsEtAl))*nrow(WetzelsEtAl))
    res <- computeBFWetzelsBFDA(WetzelsEtAl[rws,], ExpertsPriors_tTest)
    BFWetzels[rws,] <- res
  }
  return(BFWetzels)
}

# All in one function

#' Function to compute all BF with experts' priors for the Wetzels dataset
#' @param WetzelsEtAl The Wetzels et al dataset (as constructed in /data-raw)
#' @param ExpertsPriors_tTest The ExpertsPriors_tTest dataset (as constructed in /data-raw)
#' @export

computeBFWetzels_complete <- function(WetzelsEtAl, ExpertsPriors_tTest){
  BFWetzels <- computeBFWetzels(WetzelsEtAl, ExpertsPriors_tTest)
  BFWetzels <- recompute_BF_if_numeric_problems(BFWetzels, WetzelsEtAl, ExpertsPriors_tTest)
  return(BFWetzels)
}
