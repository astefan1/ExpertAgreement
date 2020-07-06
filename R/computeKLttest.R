#### Compute KL divergence between informed and default priors for Wetzels et al. dataset ####

# Prepare KLdiv_cor function so that it runs on rows of WetzelsEtAl and prior data
KLWetzels <- function(dat, prior, direction, stage){

  dat <- suppressWarnings(as.numeric(dat))
  prior <- suppressWarnings(as.numeric(prior))

  if(stage=="MATCH"){
    KLMATCHt <- KLdiv_tTest_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                              prior.location=prior[25], prior.scale=prior[26], prior.df=prior[27], direction=direction)
    KLMATCHnorm <- KLdiv_tTest_norm(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                    prior.mean=prior[23], prior.variance=prior[24]^2, direction=direction)
    res <- c(KLMATCHt, KLMATCHnorm)
    names(res) <- c("KL_MATCH_t", "KL_MATCH_norm")

  } else if(stage=="Shiny"){
    KLShinyt <- KLdiv_tTest_t(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                              prior.location=prior[31], prior.scale=prior[32], prior.df=prior[33], direction=direction)
    KLShinynorm <- KLdiv_tTest_norm(t=dat[3], n1=dat[1], n2=dat[2], independentSamples=as.logical(dat[6]),
                                    prior.mean=prior[29], prior.variance=prior[30]^2, direction=direction)
    res <- c(KLShinyt, KLShinynorm)
    names(res) <- c("KL_Shiny_t", "KL_Shiny_norm")
  }

  return(res)

}

# Compute KL divergence for every dataset in Wetzels et al.
computeKLWetzels <- function(WetzelsEtAl, ExpertsPriors_tTest, stage, direction="greater"){

  res <- matrix(NA, nrow = 0, ncol = nrow(WetzelsEtAl))

  # KLs for each expert
  for(i in 1:nrow(ExpertsPriors_cor)){
    print(i)
    tmp <- simplify2array(apply(WetzelsEtAl,
                                MARGIN = 1,
                                function(x) KLWetzels(x, ExpertsPriors_tTest[i,],
                                                      direction=direction, stage=stage)))
    rownames(tmp) <- paste0(rownames(tmp), "_Exp", i)
    res <- rbind(res, tmp)
  }

  res.t <- t(res)

}

#' Function to make a random draw from the BoscoEtAl dataset, analyze the correlations, and output the result matrix
#' @param BoscoEtAl The Bosco et al. dataset (as constructed in /data-raw)
#' @param ExpertsPriors_cor The ExpertsPriors_cor dataset (as constructed in /data-raw)
#' @param seed Random seed for random draw of studies from the Bosco et al. dataset

KLBosco_complete <- function(BoscoEtAl, ExpertsPriors_cor, seed){

  # Sample the studies
  set.seed(seed)
  studysample <- sample(unique(BoscoEtAl$studyID), 855, replace = F)
  ids <- NULL
  for(i in studysample){
    dat <- BoscoEtAl[BoscoEtAl$studyID == i,]
    ids[which(studysample==i)] <- dat$RowID[sample(length(dat$RowID), 1)]
  }
  BoscoEtAlRed <- BoscoEtAl[BoscoEtAl$RowID %in% ids,]

  # Compute the Bayes factors
  res <- computeKLBosco(BoscoEtAlRed, ExpertsPriors_cor)

  return(res)

}

