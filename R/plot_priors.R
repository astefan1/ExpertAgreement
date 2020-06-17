#### Plot Prior Distributions ####

#' @import ggsci

plotpriors_t <- function(ExpertsPriors_tTest, ylim){
  cols <- ggsci::pal_aaas("default")(9)
  expert_name <- c("Social 1", "Social 2", "Neuro 1", "Neuro 2",
                   "Developmental 1", "Developmental 2")

  par(mfrow=c(3,2), mar = c(5, 5, 2, 2.1), oma = c(4, 1, 1, 0))

  for(i in 1:nrow(ExpertsPriors_tTest)){
    priors <- as.numeric(ExpertsPriors_tTest[i, c(23:27, 29:33)])
    pts <- seq(0, 1, by = 0.001)
    plot(pts, dnorm(pts, priors[1], priors[2]), ylab = "Density", xlab = bquote(delta),
         bty="l", type = "l", lwd=2,col=cols[3], ylim = ylim, lty="dotted", las=1,
         cex.axis=1.5, cex.lab=1.7)
    points(pts, dtss(pts, priors[3], priors[4], priors[5]), type = "l", lwd=2,
           col=cols[4], lty="dotted")
    points(pts, dnorm(pts, priors[6], priors[7]), type = "l", lwd=2,
           col=cols[3])
    points(pts, dtss(pts, priors[8], priors[9], priors[10]), type = "l", lwd=2,
           col=cols[4])
    mtext(expert_name[i], cex=1.2)
  }

  legend(x = -0.7, y = -2.5,
         legend = c("t-distribution ", "normal distribution"),
         lty = c("solid", "solid"), bty = "n", horiz = TRUE, lwd=c(2,2),
         xpd = NA, col = cols[c(4,3)], adj = 0,
         text.width = c(0.30, 0.30), cex=1.5)

}

plotpriors_cor <- function(ExpertsPriors_cor, ylim){
  cols <- ggsci::pal_aaas("default")(9)
  expert_name <- c("Social 1", "Social 2", "Neuro 1", "Neuro 2",
                   "Developmental 1", "Developmental 2")

  par(mfrow=c(3,2), mar = c(5, 5, 2, 2.1), oma = c(1, 1, 1, 0))

  for(i in 1:nrow(ExpertsPriors_tTest)){
    priors <- as.numeric(ExpertsPriors_cor[i, c(23:24, 26:27)])
    pts <- seq(0, 1, by = 0.001)
    plot(pts, dbeta(pts, priors[1], priors[2]), ylab = "Density", xlab = bquote(rho),
         bty="l", type = "l", lwd=2,col=cols[1], ylim = ylim, lty="dotted", las=1,
         cex.axis=1.5, cex.lab=1.7)
    points(pts, dbeta(pts, priors[3], priors[4]), type = "l", lwd=2,
           col=cols[1], lty="solid")
    mtext(expert_name[i], cex=1.2)
  }

}
