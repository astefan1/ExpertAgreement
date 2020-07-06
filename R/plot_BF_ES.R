# Plot Bayes factors for increasing sample effect sizes #

plotBFWetzelsES <- function(WetzelsEtAl, BFWetzels, ExpertPriors_tTest, elicit.stage, alternative, disttype){

  # Define arguments
  alternative <- switch(alternative,
                        "two.sided"="BF10", "greater"="BFplus0", "less"="BFmin0")
  a <- switch(alternative, "BF10"=-Inf, "BFplus0"=0, "BFmin0"=-Inf)
  b <- switch(alternative, "BF10"=Inf, "BFplus0"=Inf, "BFmin0"=0)

  # Select columns from Bayes factor dataset
  BFselect <- paste0("BF_", elicit.stage, "_", disttype, "_", alternative)
  BFelicited <- grep(BFselect, colnames(BFWetzels))
  BFdefault <- grep(paste0(alternative, "_default"), colnames(BFWetzels))
  colselect <- c(BFelicited, BFdefault)

  # Determine credible intervals of priors to find plausible effect size ranges
  if(disttype=="norm"){

    cis <- matrix(NA, nrow=0, ncol=2)
    priorpar <- as.numeric(ExpertsPriors_tTest[i, 29:30])
    for(i in 1:6){
      ci_exp <- c(truncnorm::qtruncnorm((1-0.8)/2, a=a, b=b, mean=priorpar[1], sd=priorpar[2]),
                  truncnorm::qtruncnorm(0.8+(1-0.8)/2, a=a, b=b, mean=priorpar[1], sd=priorpar[2]))
      cis <- rbind(cis, ci_exp)
    }
  }

}
