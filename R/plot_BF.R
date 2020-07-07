#' Plot Bayes factors for increasing sample effect sizes in the Wetzels et al data
#' @param WetzelsEtAl Wetzels et al. dataset
#' @param BFWetzels Dataset containing Bayes factors for Wetzels data
#' @param ExpertsPriors_tTest Dataset ExpertsPriors_tTest containing prior distribution data for the t-test
#' @param elicit.stage "Shiny" or "MATCH" for elicited priors from Shiny app or MATCH
#' @param alternative one out of "two.sided", "greater", or "less"
#' @param disttype Which prior distribution family? "norm" for normal distribution, "t" for t-distribution
#' @param percent Determines xlim bounds. Credible interval of the prior distributions.

plotBFWetzelsES <- function(WetzelsEtAl, BFWetzels, ExpertsPriors_tTest, elicit.stage, alternative, disttype, percent=0.3){

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

  # Determine 80% credible intervals of priors to find plausible effect size ranges
  cis <- matrix(NA, nrow=0, ncol=2)

  if(disttype=="norm"){
    normcols <- switch(elicit.stage, "MATCH"=c(23:24), "Shiny"=c(29:30))
    for(i in 1:6){
      priorpar <- as.numeric(ExpertsPriors_tTest[i, normcols])
      ci_exp <- c(truncnorm::qtruncnorm((1-percent)/2, a=a, b=b, mean=priorpar[1], sd=priorpar[2]),
                  truncnorm::qtruncnorm(percent+(1-percent)/2, a=a, b=b, mean=priorpar[1], sd=priorpar[2]))
      cis <- rbind(cis, ci_exp)
    }

  } else if(disttype=="t"){

    tcols <- switch(elicit.stage, "MATCH"=c(25:27), "Shiny"=c(31:33))
    for(i in 1:6){
      priorpar <- as.numeric(ExpertsPriors_tTest[i, tcols])
      ci_exp <- c(qtss_trunc((1-percent)/2, a=a, b=b, mu=priorpar[1], r=priorpar[2], kappa=priorpar[3]),
                  qtss_trunc(percent+(1-percent)/2, a=a, b=b, mu=priorpar[1], r=priorpar[2], kappa=priorpar[3]))
      cis <- rbind(cis, ci_exp)
    }
  }
  cis <- rbind(c(-1, 0), cis, c(1, 2))

  # Order WetzelsEtAl and BFWetzels according to ES
  WetzelsEtAlOrdered <- WetzelsEtAl[order(WetzelsEtAl$effectsize),]
  BFWetzelsOrdered <- BFWetzels[order(WetzelsEtAl$effectsize),]

  # Plot BF within ES ranges
  cols <- ggsci::pal_uchicago("light", alpha = 0.8)(9)[c(1:5,9)]
  cols <- c(cols, "black")
  path <- paste0("../Figures/plot_BFWetzels", "_", elicit.stage, "_", alternative, "_", disttype, ".pdf")
  grDevices::pdf(file = path, width = 9.2, height=7.2)
  par(mar=c(7,5,1,1))
  for(i in 1:nrow(cis)){
    xval <- WetzelsEtAlOrdered$effectsize[WetzelsEtAlOrdered$effectsize <= cis[i, 2] & WetzelsEtAlOrdered$effectsize >= cis[i, 1]]
    yval <- BFWetzelsOrdered[WetzelsEtAlOrdered$effectsize <= cis[i, 2] & WetzelsEtAlOrdered$effectsize >= cis[i, 1], colselect]
    xlim <- cis[i,]
    ylim <- round(c(min(log(as.numeric(unlist(yval)))), max(log(as.numeric(unlist(yval))))), 2)
    plot(xval, log(as.numeric(yval[,1])),
         pch=15, col=cols[1], xlim=xlim, cex=1, ylim=ylim,
         xlab="Effect Size", ylab="log BF", cex.lab=1.5,
         bty="l")
    for(j in 2:ncol(yval)){
      points(xval, log(as.numeric(yval[,j])), pch=15, col=cols[j], cex=1, xpd=NA)
    }
    legend(x=xlim[1]-(xlim[2]-xlim[1])/10, y=ylim[1]-(ylim[2]-ylim[1])/4.5,
           legend=c(paste("Expert", c(1:6)), "Default Prior"),
           pch=15, col=cols, bty="n", horiz=TRUE, xpd=NA)

  }
  grDevices::dev.off()

}


#' Plot Bayes factors for increasing sample effect sizes in the Bosco et al data
#' @param BoscoEtAl Bosco et al. dataset
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param ExpertsPriors_cor Dataset ExpertsPriors_tTest containing prior distribution data for the t-test
#' @param elicit.stage "Shiny" or "MATCH" for elicited priors from Shiny app or MATCH
#' @param percent Determines xlim bounds. Credible interval of the prior distributions.
#' @importFrom stats rnorm
#' @importFrom stats qbeta

plotBFBoscoES <- function(BoscoEtAl, BFBosco, ExpertsPriors_cor, elicit.stage, percent=0.3){

  # Select columns from Bayes factor dataset
  BFselect <- paste0("BF_", elicit.stage)
  BFelicited <- grep(BFselect, colnames(BFBosco))
  BFdefault <- grep("_default", colnames(BFBosco))
  colselect <- c(BFelicited, BFdefault)

  # Determine 80% credible intervals of priors to find plausible effect size ranges
  cis <- matrix(NA, nrow=0, ncol=2)
  priorcols <- switch(elicit.stage, "MATCH"=c(23:24), "Shiny"=c(26:27))
  for(i in 1:6){
    priorpar <- as.numeric(ExpertsPriors_cor[i, priorcols])
    ci_exp <- c(stats::qbeta((1-percent)/2, shape1=priorpar[1], shape2=priorpar[2]),
                stats::qbeta(percent+(1-percent)/2, shape1=priorpar[1], shape2=priorpar[2]))
    cis <- rbind(cis, ci_exp)
  }
  cis <- rbind(c(-0.5, min(cis)), cis, c(max(cis), 1))

  # Order BoscoEtAl and BFBosco according to ES
  BoscoEtAlRed <- BoscoEtAl[rownames(BFBosco), ]
  BoscoEtAlOrdered <- BoscoEtAlRed[order(BoscoEtAlRed$r),]
  BFBoscoOrdered <- BFBosco[order(BoscoEtAlRed$r),]

  # Plot BF within ES ranges
  cols <- ggsci::pal_uchicago("light", alpha = 0.8)(9)[c(1:5,9)]
  cols <- c(cols, "black")

  path <- paste0("../Figures/plot_BFBosco", "_", elicit.stage, ".pdf")
  grDevices::pdf(file = path, width = 9.2, height=7.2)

  par(mar=c(7,5,1,1))
  for(i in 1:nrow(cis)){
    xval <- BoscoEtAlOrdered$r[BoscoEtAlOrdered$r <= cis[i, 2] & BoscoEtAlOrdered$r >= cis[i, 1]]
    xval <- xval+stats::rnorm(length(xval), 0, 0.001)
    yval <- BFBoscoOrdered[BoscoEtAlOrdered$r <= cis[i, 2] & BoscoEtAlOrdered$r >= cis[i, 1], colselect]
    xlim <- cis[i,]
    ylim <- round(c(min(log(as.numeric(unlist(yval)))), max(log(as.numeric(unlist(yval))))), 2)
    plot(xval, log(as.numeric(yval[,1])),
         pch=15, col=cols[1], xlim=xlim, cex=1, ylim=ylim,
         xlab="Effect Size", ylab="log BF", cex.lab=1.5,
         bty="l")
    for(j in 2:ncol(yval)){
      points(xval, log(as.numeric(yval[,j])), pch=15, col=cols[j], cex=1, xpd=NA)
    }
    legend(x=xlim[1]-(xlim[2]-xlim[1])/10, y=ylim[1]-(ylim[2]-ylim[1])/4.5,
           legend=c(paste("Expert", c(1:6)), "Default Prior"),
           pch=15, col=cols, bty="n", horiz=TRUE, xpd=NA)

  }
  grDevices::dev.off()

}
