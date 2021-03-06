% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_plot_BF.R
\name{plotBFWetzels}
\alias{plotBFWetzels}
\title{Plot BF for the Wetzels et al dataset}
\usage{
plotBFWetzels(
  BFWetzels,
  disttype = c("norm", "t"),
  elicit.stage = c("MATCH", "Shiny"),
  alternative = c("two.sided", "greater", "less"),
  BFdir = c("H0", "H1", "Mixed"),
  pdf.width = 8.5,
  pdf.height = 4,
  cex.text = 1.5,
  mar = c(5, 3, 1, 1),
  line.ylab = 1.5,
  xtck = NULL
)
}
\arguments{
\item{BFWetzels}{Dataset containing BF of Wetzels et al. data}

\item{disttype}{Which type of prior distribution? "norm" = normal, "t" = t-distribution}

\item{elicit.stage}{Which stage of the elicitation process? "MATCH" = MATCH results, "Shiny" = Shiny results}

\item{alternative}{Direction of Bayesian test: "two-sided" = two-sided test, "greater" = one-sided test delta > 0, "less" = one-sided test delta < 0}

\item{BFdir}{Which studies should be looked at? "H0" = all BFs point towards H0, "H1" = all BFs point towards H1, "Mixed" = direction of BF changes}

\item{pdf.width}{Width of the pdf output}

\item{pdf.height}{Height of the pdf output}

\item{cex.text}{Font size of axis labels}

\item{mar}{Margins of the plot to be inserted in par(mar=...)}

\item{line.ylab}{Position of y-axis label relative to y axis (to be inserted in mtext(line=...))}

\item{xtck}{Vector specifying location of tick marks on x-axis, automatically determined if xtcks=NULL (default)}
}
\description{
Plot BF for the Wetzels et al dataset
}
