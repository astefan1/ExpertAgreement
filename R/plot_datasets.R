#### Descriptive plots of datasets ####

plotdata <- function(WetzelsEtAl, BoscoEtAl, seed=1234){

  grDevices::pdf(file = "../Figures/plot_data.pdf", width = 9.2, height=5.2)

  par(mfrow=c(1,2))
  # Wetzels et al
  plot(WetzelsEtAl$effectsize, WetzelsEtAl$N1,
       pch=21, bg=scales::alpha("grey26", 0.5), col="black",
       xlab="", ylab="", bty="l", las=1)
  mtext("Sample Size (per group)", side=2, cex=1.2, line=3)
  mtext(bquote("Effect Size "*delta), side=1, cex=1.2, line=3)
  mtext("Data: Wetzels et al. (2011)", side=3, cex=1.5, line=1.5)

  # Bosco et al

  ## reduced dataset
  set.seed(seed)
  studysample <- sample(unique(BoscoEtAl$studyID), 855, replace = F)
  ids <- NULL
  for(i in studysample){
    dat <- BoscoEtAl[BoscoEtAl$studyID == i,]
    ids[which(studysample==i)] <- dat$RowID[sample(length(dat$RowID), 1)]
  }
  BoscoEtAlRed <- BoscoEtAl[BoscoEtAl$RowID %in% ids,]

  ## plot
  plot(BoscoEtAlRed$r, BoscoEtAlRed$N,
       pch=21, bg=scales::alpha("grey26", 0.5), col="black",
       xlab="", ylab="", bty="l", las=1)
  mtext("Sample Size", side=2, cex=1.2, line=3)
  mtext(bquote("Effect Size "*rho), side=1, cex=1.2, line=3)
  mtext("Data: Bosco et al. (2015)", side=3, cex=1.5, line=1.5)

  grDevices::dev.off()
}
