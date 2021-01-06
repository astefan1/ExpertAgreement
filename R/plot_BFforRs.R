#' Plot Bayes factors for increasing sample effect sizes in the Bosco et al data
#' @param BoscoEtAl Bosco et al. dataset
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param ExpertsPriors_cor Dataset ExpertsPriors_tTest containing prior distribution data for the t-test
#' @param elicit.stage "Shiny" or "MATCH" for elicited priors from Shiny app or MATCH
#' @param whichplots For which experts should plots be created? (1=negative ES, 2:7=experts, 8=large positive ES)
#' @importFrom stats rnorm
#' @importFrom stats qbeta

plotBFBoscoOneR <- function(BoscoEtAl, BFBosco, ExpertsPriors_cor, elicit.stage, whichplots=c(1,3,7,8)){

  # Select columns from Bayes factor dataset
  BFselect <- paste0("BF_", elicit.stage)
  BFelicited <- grep(BFselect, colnames(BFBosco))
  BFdefault <- grep("_default", colnames(BFBosco))
  colselect <- c(BFelicited, BFdefault)

  # Order BoscoEtAl and BFBosco according to ES
  BoscoEtAlRed <- BoscoEtAl[rownames(BFBosco), ]
  BoscoEtAlOrdered <- BoscoEtAlRed[order(BoscoEtAlRed$r),]
  BFBoscoOrdered <- BFBosco[order(BoscoEtAlRed$r),]

  # Plot BF within ES ranges
  cols <- ggsci::pal_uchicago("light", alpha = 0.8)(9)[c(1:5,9)]
  cols <- c(cols, "black")

  cis <- c(-0.200, 0.010, 0.200, 0.250, 0.310, 0.290, 0.400, 0.600)
  path <- paste0("../Figures/BFforDifferentRs.pdf")
  grDevices::pdf(file = path, width = 8.37, height=7.26)
  par(mfrow=c(2,2), mar=c(5,5,4,1), oma=c(3,0,0,0))
  for(i in whichplots){
    xval <- BoscoEtAlOrdered$N[BoscoEtAlOrdered$r == cis[i]]
    yval <- BFBoscoOrdered[BoscoEtAlOrdered$r == cis[i], colselect]
    ylim <- round(c(min(log(as.numeric(unlist(yval)))), max(log(as.numeric(unlist(yval))))), 2)
    plot(xval, log(as.numeric(yval[,1])),
         pch=19, col=cols[1], cex=1.5, ylim=ylim,
         xlab="Sample Size", ylab=bquote("log BF"[10]), cex.lab=1.5,
         bty="l", las=1, cex.axis=1.3)
    for(j in 2:ncol(yval)){
      points(xval, log(as.numeric(yval[,j])), pch=19, col=cols[j], cex=1.5, xpd=NA)
    }
    legend(x="topright", legend=paste0("r = ", cis[i]), inset=c(0,-0.3), xpd=NA, cex=1.5, adj=0.2)
    mtext(LETTERS[which(whichplots==i)], side = 3, adj=0, cex=2, line = 1.5)
  }
  legend(x="bottomleft", inset=c(-1.55, -0.7), legend=c(paste("Expert", c(1:6)), "Default Prior"), pch=19, col=cols, bty="n", horiz=TRUE, xpd=NA, cex=1.2, x.intersp = 0.5, pt.cex=1.5)
  grDevices::dev.off()
}
