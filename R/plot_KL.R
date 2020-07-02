#' Plot KL divergence for the Bosco et al dataset
#' @param BFBosco Dataset containing BF of Bosco et al. data
#' @param elicit.stage Which stage of the elicitation process? "MATCH" = MATCH results, "Shiny" = Shiny results
#' @param BFdir Which studies should be looked at? "H0" = all BFs point towards H0, "H1" = all BFs point towards H1, "Mixed" = direction of BF changes
#' @param pdf.width Width of the pdf output
#' @param pdf.height Height of the pdf output
#' @param cex.text Font size of axis labels
#' @param mar Margins of the plot to be inserted in par(mar=...)
#' @param line.ylab Position of y-axis label relative to y axis (to be inserted in mtext(line=...))
#' @param xtck Vector specifying location of tick marks on x-axis, automatically determined if xtcks=NULL (default)
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#' @importFrom ggsci pal_uchicago

plotBFBosco <- function(BFBosco,
                        elicit.stage=c("MATCH", "Shiny"),
                        BFdir=c("H0", "H1", "Mixed"),
                        pdf.width=8.5,
                        pdf.height=4,
                        cex.text=1.5,
                        mar=c(5,3,1,1),
                        line.ylab=1.5,
                        xtck=NULL){

  # PDF settings
  path <- paste0("../Figures/plot_BFBosco", "_", elicit.stage, "_", BFdir, ".pdf")
  grDevices::pdf(file = path, width = pdf.width, height=pdf.height)

  # Select the columns for plotting
  BFelicited <- grep(paste0("BF_", elicit.stage), colnames(BFBosco))
  BFdefault <- grep("default", colnames(BFBosco))
  colselect <- c(BFelicited, BFdefault)

  # select rows for plotting
  howmanyBFH1 <- rowSums(BFBosco[, colselect]>1)
  BFdirNum <- switch(BFdir,
                     "H0"=0, "H1"=7, "Mixed"=c(1:6))
  if(length(BFdirNum)==1){
    BFBoscoRed <- BFBosco[howmanyBFH1==BFdirNum, colselect]
  } else {
    BFBoscoRed <- BFBosco[howmanyBFH1 %in% BFdirNum, colselect]
  }

  # Prepare plotting objects and parameters
  N <- ifelse(nrow(BFBoscoRed)>100, 100, nrow(BFBoscoRed))
  if(BFdir=="H0"){
    yval <- log(1/BFBoscoRed[1:N, ])
    ylab <- bquote("log BF"["01"])
  } else {
    yval <- log(BFBoscoRed[1:N, ])
    ylab <- bquote("log BF"["10"])
  }
  ylim <- c(min(0, min(yval)), max(yval))
  ytck <- pretty(ylim, min.n=3)
  if(length(xtck)==0){
    xtck <- pretty(c(1,N), min.n=3)
    xtck[1] <- 1
  }
  cols <- ggsci::pal_uchicago("light", alpha = 1)(9)

  # Plot starts here
  par(mar=mar)
  plot(c(1:N), yval[,1] , pch=15, col=cols[1], ylim=ylim,
       xlim=c(1,N), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  apply(as.array(c(1:6)), MARGIN=1,
        FUN= function(x){
          cols <- c(cols[c(2:5, 9)], "black")
          points(c(1:N), yval[, x+1], pch=15, col=cols[x])
        })
  axis(side=1, at=xtck)
  axis(side=2, pos=0, at=ytck, las=1)
  apply(as.array(c(1:(N+1))), MARGIN = 1,
        FUN = function(x) abline(v=x-0.5, col="grey", lwd=0.5))
  mtext(ylab, 2, cex=cex.text, line=line.ylab)
  mtext("Study Number", 1, cex=cex.text, line=3)
  if(BFdir=="Mixed") segments(1, 0, N, 0)
  grDevices::dev.off()
}


#' Plot BF for the Wetzels et al dataset
#' @param KLWetzels Dataset containing BF of Wetzels et al. data
#' @param disttype Which type of prior distribution? "norm" = normal, "t" = t-distribution
#' @param elicit.stage Which stage of the elicitation process? "MATCH" = MATCH results, "Shiny" = Shiny results
#' @param pdf.width Width of the pdf output
#' @param pdf.height Height of the pdf output
#' @param cex.text Font size of axis labels
#' @param mar Margins of the plot to be inserted in par(mar=...)
#' @param line.ylab Position of y-axis label relative to y axis (to be inserted in mtext(line=...))
#' @param xtck Vector specifying location of tick marks on x-axis, automatically determined if xtcks=NULL (default)
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#' @importFrom ggsci pal_uchicago

plotKLWetzels <- function(KLWetzels,
                          disttype=c("norm","t"),
                          elicit.stage=c("MATCH", "Shiny"),
                          pdf.width=8.5,
                          pdf.height=4,
                          cex.text=1.5,
                          mar=c(5,3,1,1),
                          line.ylab=1.5,
                          xtck=NULL){

  # PDF settings
  path <- paste0("../Figures/plot_BFWetzels", "_", elicit.stage, "_", disttype, ".pdf")
  grDevices::pdf(file = path, width = pdf.width, height=pdf.height)

  # Select the columns for plotting
  BFselect <- paste0("BF_", elicit.stage, "_", disttype, "_", alternative)
  BFelicited <- grep(BFselect, colnames(BFWetzels))
  BFdefault <- grep(paste0(alternative, "_default"), colnames(BFWetzels))
  colselect <- c(BFelicited, BFdefault)

  # select rows for plotting
  howmanyBFH1 <- rowSums(BFWetzels[, colselect]>1)
  BFdirNum <- switch(BFdir,
                     "H0"=0, "H1"=7, "Mixed"=c(1:6))
  if(length(BFdirNum)==1){
    BFWetzelsRed <- BFWetzels[howmanyBFH1==BFdirNum, colselect]
  } else {
    BFWetzelsRed <- BFWetzels[howmanyBFH1 %in% BFdirNum, colselect]
  }

  # Prepare plotting objects and parameters
  N <- ifelse(nrow(BFWetzelsRed)>100, 100, nrow(BFWetzelsRed))
  BFWetzelsRed <- apply(BFWetzelsRed, 2, as.numeric)
  if(BFdir=="H0"){
    yval <- log(1/BFWetzelsRed[1:N, ])
    ylab <- bquote("log BF"["01"])
  } else {
    yval <- log(BFWetzelsRed[1:N, ])
    ylab <- bquote("log BF"["10"])
  }
  ylim <- c(min(0, min(yval)), max(yval))
  ytck <- pretty(ylim, min.n=3)
  if(length(xtck)==0){
    xtck <- pretty(c(1,N), min.n=3)
    xtck[1] <- 1
  }
  cols <- ggsci::pal_uchicago("light", alpha = 1)(9)

  # Plot starts here
  par(mar=mar)
  plot(c(1:N), yval[,1] , pch=15, col=cols[1], ylim=ylim,
       xlim=c(1,N), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  apply(as.array(c(1:6)), MARGIN=1,
        FUN= function(x){
          cols <- c(cols[c(2:5, 9)], "black")
          points(c(1:N), yval[, x+1], pch=15, col=cols[x])
        })
  axis(side=1, at=xtck)
  axis(side=2, pos=0, at=ytck, las=1)
  apply(as.array(c(1:(N+1))), MARGIN = 1,
        FUN = function(x) abline(v=x-0.5, col="grey", lwd=0.5))
  mtext(ylab, 2, cex=cex.text, line=line.ylab)
  mtext("Study Number", 1, cex=cex.text, line=3)
  if(BFdir=="Mixed") segments(1, 0, N, 0)
  grDevices::dev.off()
}
