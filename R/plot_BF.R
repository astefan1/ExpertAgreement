#### Plot Bayes factors Bosco et al dataset ####

#' Plot BF for the Bosco et al dataset where all BF point towards H0
#' @param BFBosco BFBosco dataset containing the BF of the Bosco et al study
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off

plotBFBoscoH0 <- function(BFBosco){
  grDevices::pdf(file = "../Figures/plot_BFBoscoH0.pdf",
      width =  8.5,
      height = 4)
  cols <- ggsci::pal_uchicago("light", alpha = 1)(9)

  howmanyBFH1 <- rowSums(BFBosco[, c(2,4,6,8,10,12)]>1)

  BFBoscoH0 <- BFBosco[howmanyBFH1==0, ]

  par(mar=c(5,3,1,1))
  plot(c(1:100), log(1/BFBoscoH0[1:100,2]), pch=15, col=cols[1], ylim=c(0,18),
       xlim=c(1,100), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  apply(as.array(c(1:6)), MARGIN=1,
        FUN= function(x){
          expert <- c(4,6,8,10,12,13)
          cols <- c(cols[c(2:5, 9)], "black")
          points(c(1:100), log(1/BFBoscoH0[1:100,expert[x]]), pch=15, col=cols[x])
        })
  axis(side=1, at=c(1,seq(10,100,by=10)))
  axis(side=2, pos=0, at=seq(0,20,by=5), las=1)
  apply(as.array(c(1:101)), MARGIN = 1, FUN = function(x) abline(v=x-0.5, col="grey", lwd=0.5))
  mtext(bquote("log BF"["01"]), 2, cex=1.5, line=1.5)
  mtext("Study Number", 1, cex=1.5, line=3)
  grDevices::dev.off()
}

#' Plot BF for the Bosco et al dataset where all BF point towards H1
#' @param BFBosco BFBosco dataset containing the BF of the Bosco et al study
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off

plotBFBoscoH1 <- function(BFBosco){

  grDevices::pdf(file = "../Figures/plot_BFBoscoH1.pdf",
      width =  9,
      height = 7)

  cols <- ggsci::pal_uchicago("light", alpha = 1)(9)

  howmanyBFH1 <- rowSums(BFBosco[, c(2,4,6,8,10,12)]>1)

  BFBoscoH1 <- BFBosco[howmanyBFH1==6, ]

  par(mar=c(5,3,1,1))
  plot(c(1:100), log(BFBoscoH1[1:100,2]), pch=15, col=cols[1], ylim=c(0,260),
       xlim=c(1,100), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  apply(as.array(c(1:6)), MARGIN=1,
        FUN= function(x){
          expert <- c(4,6,8,10,12,13)
          cols <- c(cols[c(2:5, 9)], "black")
          points(c(1:100), log(BFBoscoH1[1:100,expert[x]]), pch=15, col=cols[x])
        })
  axis(side=1, at=c(1,seq(10,100,by=10)))
  axis(side=2, pos=0, at=seq(0,260,by=50), las=1)
  apply(as.array(c(1:101)), MARGIN = 1, FUN = function(x) abline(v=x-0.5, col="grey", lwd=0.5))
  mtext(bquote("log BF"["10"]), 2, cex=1.5, line=1.5)
  mtext("Study Number", 1, cex=1.5, line=3)

  grDevices::dev.off()

}

#' Plot BF for the Bosco et al dataset where BF point to H0 or H1 depending on the prior
#' @param BFBosco BFBosco dataset containing the BF of the Bosco et al study
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off

plotBFBoscoMixed <- function(BFBosco){
  grDevices::pdf(file = "../Figures/plot_BFBoscoMixed.pdf",
      width =  8,
      height = 5)

  cols <- ggsci::pal_uchicago("light", alpha = 1)(9)

  howmanyBFH1 <- rowSums(BFBosco[, c(2,4,6,8,10,12)]>1)

  BFBoscoMixed <- BFBosco[howmanyBFH1 %in% c(1:5), ]

  par(mar=c(5,3,1,1))
  plot(c(1:100), log(BFBoscoMixed[1:100,2]), pch=15, col=cols[1], ylim=c(-10,15),
       xlim=c(1,100), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  apply(as.array(c(1:6)), MARGIN=1,
        FUN= function(x){
          expert <- c(4,6,8,10,12,13)
          cols <- c(cols[c(2:5, 9)], "black")
          points(c(1:100), log(BFBoscoMixed[1:100,expert[x]]), pch=15, col=cols[x])
        })
  axis(side=1, at=c(1,seq(10,100,by=10)))
  axis(side=2, pos=0, at=seq(-10,15,by=5), las=1)
  apply(as.array(c(1:101)), MARGIN = 1, FUN = function(x) abline(v=x-0.5, col="grey", lwd=0.5))
  mtext(bquote("log BF"["10"]), 2, cex=1.5, line=1.5)
  mtext("Study Number", 1, cex=1.5, line=3)
  segments(0,0, x1=100)

  grDevices::dev.off()
}



