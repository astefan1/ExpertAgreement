#### Compute KL divergence between informed and default priors for Bosco et al. dataset ####

# Prepare KLdiv_cor function so that it runs on rows of BoscoEtAl and prior data
KLBosco <- function(dat, prior){

  dat <- suppressWarnings(as.numeric(dat))
  prior <- suppressWarnings(as.numeric(prior))

  KLMATCH <- KLdiv_cor(N=dat[8], R=dat[6], prior[23], prior[24])
  KLShiny <- KLdiv_cor(N=dat[8], R=dat[6], prior[26], prior[27])

  res <- c(KLMATCH, KLShiny)
  names(res) <- c("KL_MATCH", "KL_Shiny")

  return(res)

}

# Compute KL divergence for every dataset in BoscoEtAl
computeKLBosco <- function(BoscoEtAl, ExpertsPriors_cor){

  res <- matrix(NA, nrow = 0, ncol = nrow(BoscoEtAl))

  # KLs for each expert
  for(i in 1:nrow(ExpertsPriors_cor)){
    print(i)
    tmp <- simplify2array(apply(BoscoEtAl,
                                MARGIN = 1,
                                function(x) KLBosco(x, ExpertsPriors_cor[i,])))
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

