
scatterBF <- function(x, y, x1, y1, x0, y0){
  plot(x, y,
       xlim=c(x0,x1), ylim=c(y0,y1),
       pch=21, bg=scales::alpha("grey26", 0.5), col="black")
  segments(x0=x0, y0=y0, x1=x1, y1=y1, cex=1.1)
}

#' Bayes factor correspondence plot for Wetzels et al data
#' @param BFWetzels Dataset containing Bayes factors for Wetzels data
#' @param elicit.stage "Shiny" or "MATCH" for elicited priors from Shiny app or MATCH
#' @param alternative one out of "two.sided", "greater", or "less"
#' @param disttype Which prior distribution family? "norm" for normal distribution, "t" for t-distribution
#' @param BFlog TRUE/FALSE: Should log BFs be displayed?
#' @param x1 upper limit x axis
#' @param y1 upper limit y axis
#' @param x0 lower limit x axis
#' @param y0 lower limit y axis

plotallBFWetzels <- function(BFWetzels, alternative="greater", elicit.stage="Shiny", disttype="t", BFlog=FALSE, x1=100, y1=100, x0=0, y0=0){

  # Define arguments
  alternative <- switch(alternative,
                        "two.sided"="BF10", "greater"="BFplus0", "less"="BFmin0")

  # Select columns from Bayes factor dataset
  BFselect <- paste0("BF_", elicit.stage, "_", disttype, "_", alternative)
  BFelicited <- grep(BFselect, colnames(BFWetzels))
  BFdefault <- grep(paste0(alternative, "_default"), colnames(BFWetzels))
  colselect <- c(BFelicited, BFdefault)

  # Plot log BFs?
  if(BFlog){
    BFWetzelslog <- apply(BFWetzels, 2, as.numeric)
    BFWetzelslog <- log(BFWetzelslog)
    BFWetzels <- BFWetzelslog
  }

  path <- paste0("../Figures/allBFWetzels_log", BFlog, ".pdf")
  grDevices::pdf(file = path, width = 8.37, height=7.26)

  # Plot everything
  par(mfrow=c(6,6), mar=c(1,0,0,1), oma=c(3,3,1,2), xaxt="n", yaxt="n")
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  scatterBF(BFWetzels[, colselect[7]], BFWetzels[, colselect[6]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  mtext(side=4, "Expert 6", line=1.5, cex=1.3)
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  scatterBF(BFWetzels[, colselect[6]], BFWetzels[, colselect[5]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFWetzels[, colselect[7]], BFWetzels[, colselect[5]], x1, y1, x0, y0)
  mtext(side=4, "Expert 5", line=1.5, cex=1.3)
  plot.new()
  plot.new()
  plot.new()
  scatterBF(BFWetzels[, colselect[5]], BFWetzels[, colselect[4]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFWetzels[, colselect[6]], BFWetzels[, colselect[4]], x1, y1, x0, y0)
  scatterBF(BFWetzels[, colselect[7]], BFWetzels[, colselect[4]], x1, y1, x0, y0)
  mtext(side=4, "Expert 4", line=1.5, cex=1.3)
  plot.new()
  plot.new()
  scatterBF(BFWetzels[, colselect[4]], BFWetzels[, colselect[3]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFWetzels[, colselect[5]], BFWetzels[, colselect[3]], x1, y1, x0, y0)
  scatterBF(BFWetzels[, colselect[6]], BFWetzels[, colselect[3]], x1, y1, x0, y0)
  scatterBF(BFWetzels[, colselect[7]], BFWetzels[, colselect[3]], x1, y1, x0, y0)
  mtext(side=4, "Expert 3", line=1.5, cex=1.3)
  plot.new()
  scatterBF(BFWetzels[, colselect[3]], BFWetzels[, colselect[2]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFWetzels[, colselect[4]], BFWetzels[, colselect[2]], x1, y1, x0, y0)
  scatterBF(BFWetzels[, colselect[5]], BFWetzels[, colselect[2]], x1, y1, x0, y0)
  scatterBF(BFWetzels[, colselect[6]], BFWetzels[, colselect[2]], x1, y1, x0, y0)
  scatterBF(BFWetzels[, colselect[7]], BFWetzels[, colselect[2]], x1, y1, x0, y0)
  mtext(side=4, "Expert 2", line=1.5, cex=1.3)
  scatterBF(BFWetzels[, colselect[2]], BFWetzels[, colselect[1]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 2", line=2.5, cex=1.3)
  scatterBF(BFWetzels[, colselect[3]], BFWetzels[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 3", line=2.5, cex=1.3)
  scatterBF(BFWetzels[, colselect[4]], BFWetzels[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 4", line=2.5, cex=1.3)
  scatterBF(BFWetzels[, colselect[5]], BFWetzels[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 5", line=2.5, cex=1.3)
  scatterBF(BFWetzels[, colselect[6]], BFWetzels[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 6", line=2.5, cex=1.3)
  scatterBF(BFWetzels[, colselect[7]], BFWetzels[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=4, "Expert 1", line=1.5, cex=1.3)
  mtext(side=1, "Default", line=2.5, cex=1.3)

  grDevices::dev.off()
}

#' Bayes factor correspondence plot for Bosco et al data
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param elicit.stage "Shiny" or "MATCH" for elicited priors from Shiny app or MATCH
#' @param BFlog TRUE/FALSE: Should log BFs be displayed?
#' @param x1 upper limit x axis
#' @param y1 upper limit y axis
#' @param x0 lower limit x axis
#' @param y0 lower limit y axis

plotallBFBosco <- function(BFBosco, elicit.stage="Shiny", BFlog=FALSE, x1=100, y1=100, x0=0, y0=0){

  # Select columns from Bayes factor dataset
  BFselect <- paste0("BF_", elicit.stage)
  BFelicited <- grep(BFselect, colnames(BFBosco))
  BFdefault <- grep("_default", colnames(BFBosco))
  colselect <- c(BFelicited, BFdefault)

  # Plot log BFs?
  if(BFlog){
    BFBoscolog <- apply(BFBosco, 2, as.numeric)
    BFBoscolog <- log(BFBoscolog)
    BFBosco <- BFBoscolog
  }

  path <- paste0("../Figures/allBFBosco_log", BFlog, ".pdf")
  grDevices::pdf(file = path, width = 8.37, height=7.26)

  # Plot everything
  par(mfrow=c(6,6), mar=c(1,0,0,1), oma=c(3,3,1,2), xaxt="n", yaxt="n")
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  scatterBF(BFBosco[, colselect[7]], BFBosco[, colselect[6]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  mtext(side=4, "Expert 6", line=1.5, cex=1.3)
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  scatterBF(BFBosco[, colselect[6]], BFBosco[, colselect[5]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFBosco[, colselect[7]], BFBosco[, colselect[5]], x1, y1, x0, y0)
  mtext(side=4, "Expert 5", line=1.5, cex=1.3)
  plot.new()
  plot.new()
  plot.new()
  scatterBF(BFBosco[, colselect[5]], BFBosco[, colselect[4]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFBosco[, colselect[6]], BFBosco[, colselect[4]], x1, y1, x0, y0)
  scatterBF(BFBosco[, colselect[7]], BFBosco[, colselect[4]], x1, y1, x0, y0)
  mtext(side=4, "Expert 4", line=1.5, cex=1.3)
  plot.new()
  plot.new()
  scatterBF(BFBosco[, colselect[4]], BFBosco[, colselect[3]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFBosco[, colselect[5]], BFBosco[, colselect[3]], x1, y1, x0, y0)
  scatterBF(BFBosco[, colselect[6]], BFBosco[, colselect[3]], x1, y1, x0, y0)
  scatterBF(BFBosco[, colselect[7]], BFBosco[, colselect[3]], x1, y1, x0, y0)
  mtext(side=4, "Expert 3", line=1.5, cex=1.3)
  plot.new()
  scatterBF(BFBosco[, colselect[3]], BFBosco[, colselect[2]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  scatterBF(BFBosco[, colselect[4]], BFBosco[, colselect[2]], x1, y1, x0, y0)
  scatterBF(BFBosco[, colselect[5]], BFBosco[, colselect[2]], x1, y1, x0, y0)
  scatterBF(BFBosco[, colselect[6]], BFBosco[, colselect[2]], x1, y1, x0, y0)
  scatterBF(BFBosco[, colselect[7]], BFBosco[, colselect[2]], x1, y1, x0, y0)
  mtext(side=4, "Expert 2", line=1.5, cex=1.3)
  scatterBF(BFBosco[, colselect[2]], BFBosco[, colselect[1]], x1, y1, x0, y0)
  axis(side=2, tick = TRUE, yaxt="s", las=1)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 2", line=2.5, cex=1.3)
  scatterBF(BFBosco[, colselect[3]], BFBosco[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 3", line=2.5, cex=1.3)
  scatterBF(BFBosco[, colselect[4]], BFBosco[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 4", line=2.5, cex=1.3)
  scatterBF(BFBosco[, colselect[5]], BFBosco[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 5", line=2.5, cex=1.3)
  scatterBF(BFBosco[, colselect[6]], BFBosco[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=1, "Expert 6", line=2.5, cex=1.3)
  scatterBF(BFBosco[, colselect[7]], BFBosco[, colselect[1]], x1, y1, x0, y0)
  axis(side=1, tick = TRUE, xaxt="s")
  mtext(side=4, "Expert 1", line=1.5, cex=1.3)
  mtext(side=1, "Default", line=2.5, cex=1.3)

  grDevices::dev.off()

}



