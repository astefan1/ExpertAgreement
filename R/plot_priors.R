#### Plot Prior Distributions ####

#' Plot Prior Distributions for t-test
#' @param ExpertsPriors_tTest Expert prior dataset for t-test (see /data-raw)
#' @param ylim Vector of length 2; limits of the y axis
#' @import ggsci
#' @import graphics

plotpriors_t <- function(ExpertsPriors_tTest, ylim){

  grDevices::pdf(file = "../Figures/plot_priors_tTest.pdf",
                 width = 8, height=9)

  cols <- ggsci::pal_aaas("default")(9)
  expert_name <- c("Social 1", "Social 2", "Neuro 1", "Neuro 2",
                   "Developmental 1", "Developmental 2")

  graphics::par(mfrow=c(3,2), mar = c(5, 5, 2, 2.1), oma = c(4, 1, 1, 0))

  for(i in 1:nrow(ExpertsPriors_tTest)){
    priors <- as.numeric(ExpertsPriors_tTest[i, c(23:27, 29:33)])
    pts <- seq(0, 1, by = 0.001)
    plot(pts, stats::dnorm(pts, priors[1], priors[2]), ylab = "Density", xlab = bquote(delta),
         bty="l", type = "l", lwd=2,col=cols[3], ylim = ylim, lty="dotted", las=1,
         cex.axis=1.5, cex.lab=1.7)
    graphics::points(pts, dtss(pts, priors[3], priors[4], priors[5]), type = "l", lwd=2,
           col=cols[4], lty="dotted")
    graphics::points(pts, stats::dnorm(pts, priors[6], priors[7]), type = "l", lwd=2,
           col=cols[3])
    graphics::points(pts, dtss(pts, priors[8], priors[9], priors[10]), type = "l", lwd=2,
           col=cols[4])
    graphics::mtext(expert_name[i], cex=1.2)
  }

  graphics::legend(x = -0.9, y = -2.5,
         legend = c("t-distribution ", "normal distribution"),
         lty = c("solid", "solid"), bty = "n", horiz = TRUE, lwd=c(2,2),
         xpd = NA, col = cols[c(4,3)], adj = 0,
         text.width = c(0.40, 0.40), cex=1.5)

  grDevices::dev.off()

}

#' Plot Prior Distributions for t-test
#' @param ExpertsPriors_cor Expert prior dataset for correlation (see /data-raw)
#' @param ylim Vector of length 2; limits of the y axis
#' @import ggsci
#' @import graphics

plotpriors_cor <- function(ExpertsPriors_cor, ylim){

  grDevices::pdf(file = "../Figures/plot_priors_cor.pdf",
                 width = 8, height=9)

  cols <- ggsci::pal_aaas("default")(9)
  expert_name <- c("Social 1", "Social 2", "Neuro 1", "Neuro 2",
                   "Developmental 1", "Developmental 2")

  graphics::par(mfrow=c(3,2), mar = c(5, 5, 2, 2.1), oma = c(1, 1, 1, 0))

  for(i in 1:nrow(ExpertsPriors_tTest)){
    priors <- as.numeric(ExpertsPriors_cor[i, c(23:24, 26:27)])
    pts <- seq(0, 1, by = 0.001)
    plot(pts, stats::dbeta(pts, priors[1], priors[2]), ylab = "Density", xlab = bquote(rho),
         bty="l", type = "l", lwd=2,col=cols[1], ylim = ylim, lty="dotted", las=1,
         cex.axis=1.5, cex.lab=1.7)
    graphics::points(pts, stats::dbeta(pts, priors[3], priors[4]), type = "l", lwd=2,
           col=cols[1], lty="solid")
    graphics::mtext(expert_name[i], cex=1.2)
  }

  grDevices::dev.off()

}

