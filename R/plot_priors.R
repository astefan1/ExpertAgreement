#### Plot Prior Distributions ####

#' Plot Prior Distributions for t-test
#' @param ExpertsPriors_tTest Expert prior dataset for t-test (see /data-raw)
#' @param ylim Vector of length 2; limits of the y axis
#' @param disttype Normal distribution ("norm") or t-distribution ("t")
#' @import ggsci
#' @import graphics

plotpriors_t <- function(ExpertsPriors_tTest, ylim, disttype){

  grDevices::pdf(file = paste0("../Figures/plot_priors_tTest_", disttype, ".pdf"),
                 width = 8, height=9)

  cols <- ggsci::pal_uchicago("light")(9)[c(1:5,9)]
  expert_name <- c("Expert 1: Social", "Expert 2: Social", "Expert 3: Neuro", "Expert 4: Neuro ",
                   "Expert 5: Developmental", "Expert 6: Developmental")

  graphics::par(mfrow=c(3,2), mar = c(5, 5, 2, 2.1), oma = c(4, 1, 1, 0))

  for(i in 1:nrow(ExpertsPriors_tTest)){
    priors <- as.numeric(ExpertsPriors_tTest[i, c(23:27, 29:33)])
    pts <- seq(0, 1, by = 0.001)
    if(disttype=="norm"){
      yval <- stats::dnorm(pts, priors[6], priors[7])
    } else if (disttype=="t"){
      yval <- dtss(pts, priors[8], priors[9], priors[10])
    }
    plot(pts, yval, ylab = "Density", xlab = bquote(delta),
         bty="l", type = "l", lwd=2, ylim = ylim, las=1,
         cex.axis=1.5, cex.lab=1.7, col=cols[i])
    graphics::mtext(expert_name[i], cex=1.2)
  }

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

  cols <- ggsci::pal_uchicago("light")(9)[c(1:5,9)]
  expert_name <- c("Expert 1: Social", "Expert 2: Social", "Expert 3: Neuro", "Expert 4: Neuro ",
                   "Expert 5: Developmental", "Expert 6: Developmental")

  graphics::par(mfrow=c(3,2), mar = c(5, 5, 2, 2.1), oma = c(1, 1, 1, 0))

  for(i in 1:nrow(ExpertsPriors_cor)){
    priors <- as.numeric(ExpertsPriors_cor[i, c(26:27)])
    pts <- seq(0, 1, by = 0.001)
    yval <- stats::dbeta(pts, priors[1], priors[2])
    plot(pts, yval, ylab = "Density", xlab = bquote(rho),
         bty="l", type = "l", lwd=2,col=cols[i], ylim = ylim, las=1,
         cex.axis=1.5, cex.lab=1.7)
    graphics::mtext(expert_name[i], cex=1.2)
  }

  grDevices::dev.off()

}

#' Plot Default Prior Distributions
#' @import ggsci
#' @import graphics

plotpriors_default <- function(){

  grDevices::pdf(file = "../Figures/plot_priors_default.pdf",
                 width = 9.2, height=5.2)

  graphics::par(mfrow=c(1,2), mar= c(5, 4.2, 4, 2) + 0.1)
  pts <- seq(0, 1, by = 0.001)
  yt <- dtss_trunc(x=pts, mu=0, r=sqrt(2)/2, kappa=1, a=0, b=Inf)
  ycor <- dunif(x=pts, min = 0, max = 1)

  plot(pts, ycor, ylab = "Density", xlab = bquote(rho), ylim = c(0, 8),
       bty="l", type = "l", lwd=2, las=1,
       cex.axis=1.5, cex.lab=1.5)
  graphics::mtext("Correlation", cex=1.5)
  plot(pts, yt, ylab = "Density", xlab = bquote(delta), ylim = c(0, 6),
       bty="l", type = "l", lwd=2, las=1,
       cex.axis=1.5, cex.lab=1.5)
  graphics::mtext("t-Test", cex=1.5)

  grDevices::dev.off()

}
