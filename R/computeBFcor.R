#### Compute correlation Bayes factors for all datasets in the Bosco et al dataset ####

# Prepare cor_BF10 function so that it runs on BoscoEtAl and prior data
BFBosco <- function(dat, prior){

  dat <- suppressWarnings(as.numeric(dat))
  prior <- suppressWarnings(as.numeric(prior))
  BFMATCH <- cor_BF10(N=dat[8], R=dat[6], prior[23], prior[24])
  BFShiny <- cor_BF10(N=dat[8], R=dat[6], prior[26], prior[27])

  res <- c(BFMATCH, BFShiny)
  names(res) <- c("BF_MATCH", "BF_Shiny")

  return(res)
}

# Compute Bayes factors for every dataset in BoscoEtAl
computeBFBosco <- function(BoscoEtAl, ExpertsPriors_cor){

  res <- matrix(NA, nrow = 0, ncol = nrow(BoscoEtAl))

  # Informed BFs for each expert
  for(i in 1:nrow(ExpertsPriors_cor)){
    print(i)
    tmp <- simplify2array(apply(BoscoEtAl,
                                MARGIN = 1,
                                function(x) BFBosco(x, ExpertsPriors_cor[i,])))
    rownames(tmp) <- paste0(rownames(tmp), "_Exp", i)
    res <- rbind(res, tmp)
  }

  # Default BF
  BFdefault <- function(dat){
    dat <- suppressWarnings(as.numeric(dat))
    suppressWarnings(cor_BF10(N=dat[8], R=dat[6], 1, 1))
  }
  BF_default <- simplify2array(apply(BoscoEtAl,
                              MARGIN = 1,
                              function(x) BFdefault(x)))

  res <- rbind(res, BF_default)
  res.t <- t(res)
}

#' Function to make a random draw from the BoscoEtAl dataset, analyze the correlations, and output the result matrix
#' @param BoscoEtAl The Bosco et al. dataset (as constructed in /data-raw)
#' @param ExpertsPriors_cor The ExpertsPriors_cor dataset (as constructed in /data-raw)
#' @param seed Random seed for random draw of studies from the Bosco et al. dataset

BFBosco_complete <- function(BoscoEtAl, ExpertsPriors_cor, seed){

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
  res <- computeBFBosco(BoscoEtAlRed, ExpertsPriors_cor)

  return(res)

}
