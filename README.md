# ExpertAgreement

Repository containing code and data for the paper "Expert Agreement in Prior Elicitation and its Effects on Bayesian Inference" by Angelika Stefan, Dimitris Katsimpokis, Quentin Gronau, and Eric-Jan Wagenmakers.

The code is organized as an R project. To reproduce the results and figures in the paper, follow the instructions below:

### STEP 1: Read in and clean data

Download the __Bosco et al. data__ from [http://www.frankbosco.com/data/CorrelationalEffectSizeBenchmarks.html](http://www.frankbosco.com/data/CorrelationalEffectSizeBenchmarks.html) and save it to a folder. The data should be saved in a file called "Ver2-08_MasterDB_JAP-PPsych_1980-2010.xlsx".

Download the __Wetzels et al. data__ from [https://gist.github.com/richarddmorey/e44df8750d69b5c0cd8db9513e75fba4](https://gist.github.com/richarddmorey/e44df8750d69b5c0cd8db9513e75fba4). The data should be saved in a folder _Wetzels_ES-data_ that contains three files: "data_onesample.txt", "data_pairedsample.txt", and "data_twosample.txt".

Download the __parameters of the elicited priors__ from the OSF [https://osf.io/923bz/](https://osf.io/923bz/). The data should be saved in a folder _elicitation_data_ that contains the files "best_fitted_parameters.txt", "Expert1_correlation.txt", ..., "Expert6_t-test.txt".

Download [this GitHub project](https://github.com/astefan1/ExpertAgreement) and save it to a folder called ExpertAgreement.

Create a folder called _Figures_ (this will be needed later to store the figures).

Open a new R script.

Your folder structure should look like this:

- ExpertAgreement (folder containing R-code in R-package structure)
- elicitation_data (folder containing the elicitation data)
- Figures (empty folder)
- Ver2-08_MasterDB_JAP-PPsych_1980-2010.xlsx
- Wetzels_ES-data (folder containing the Wetzels data)
- NewScript.R

In the R-script, set the working directory to the ExpertAgreement folder. Run the script ExpertAgreement/data-raw/LoadAllData.R to perform the necessary data cleaning steps (see code below). The cleaned datasets will be part of the R package that is used to conduct the analyses.

```
source("data-raw/LoadAllData.R") # this may take 3-5 minutes
```

### STEP 2: Data Analysis

Follow the steps outlined in the R-script below:

```
# Load all functions from the package
devtools::load_all()

# Compute Bayes factors for Wetzels data
BFWetzels <- computeBFWetzels_complete(WetzelsEtAl, ExpertsPriors_tTest) # this may take 2-3 minutes

# Compute Bayes factors for Bosco et al data
BFBosco <- BFBosco_complete(BoscoEtAl, ExpertsPriors_cor, seed=1234) # this takes 5-10 seconds

```

### STEP 3: Plots

Follow the steps outlined in the R-script below:

```
# Plot prior distributions of experts
plotpriors_t(ExpertsPriors_tTest, ylim=c(0,6), disttype="t")
plotpriors_cor(ExpertsPriors_cor, ylim=c(0,8))
plotpriors_default()

# Plot data
plotdata(WetzelsEtAl, BoscoEtAl, seed=1234)

# Plot: What percentage of Bayes factors points in the same direction?
plotBFDirectionBosco(BFBosco, elicit.stage="Shiny") # Bosco data
plotBFDirectionWetzels(BFWetzels, elicit.stage = "Shiny", disttype = "t") # Wetzels data

# Plot: What percentage of Bayes factors share the same evidence strength?
plotEvidenceChangeBosco(BFBosco, elicit.stage="Shiny", threshold=10) # Bosco data
plotEvidenceChangeWetzels(BFWetzels, elicit.stage="Shiny", threshold=10, disttype="t") # Wetzels datq

# Plot Bayes factor values in different effect size ranges
plotBFBoscoES(BoscoEtAl, BFBosco, ExpertsPriors_cor, elicit.stage = "Shiny", percent = 1/3) # Bosco data
plotBFWetzelsES(WetzelsEtAl, BFWetzels, ExpertsPriors_tTest, elicit.stage = "Shiny",
                alternative="greater", disttype="t", percent = 1/3) # Wetzels data


```

### Appendix 

#### Analyses for Wetzels data with normal priors

```
normalpriors <- as.data.frame(ExpertsPriors_tTest[, c("mu_norm", "sigma_norm")])
rownames(normalpriors) <-  paste("Expert", c(1:6))
colnames(normalpriors) <- c("mean", "standard deviation")
normalpriors <- knitr::kable(normalpriors, format = "markdown", digits = 2,
                             caption = "Parameters of Elicited Normal Distributions")
cat(normalpriors, sep="\n", file="../Figures/TableNormalpriors.Rmd")
rmarkdown::render("../Figures/TableNormalpriors.Rmd", output_format="pdf_document")

plotpriors_t(ExpertsPriors_tTest, ylim=c(0,6), disttype="norm")
plotBFDirectionWetzels(BFWetzels, elicit.stage = "Shiny", disttype = "norm")
plotBFWetzelsES(WetzelsEtAl, BFWetzels, ExpertsPriors_tTest, elicit.stage = "Shiny",
                alternative="greater", disttype="norm", percent = 1/3)
plotEvidenceChangeWetzels(BFWetzels, elicit.stage="Shiny", threshold=10, disttype="norm")

```

### Session Info

```
> sessionInfo()
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ExpertAgreementCode_0.1.0 testthat_2.3.2            hypergeo_1.2-13          

loaded via a namespace (and not attached):
 [1] pkgload_1.1.0      sfsmisc_1.1-7      splines_4.0.2      foreach_1.5.0      elliptic_1.4-0     shiny_1.5.0       
 [7] assertthat_0.2.1   doRNG_1.8.2        remotes_2.2.0      sessioninfo_1.1.1  pillar_1.4.6       backports_1.1.10  
[13] lattice_0.20-41    glue_1.4.2         digest_0.6.25      qgam_1.3.2         promises_1.1.1     colorspace_1.4-1  
[19] logOfGamma_0.0.1   htmltools_0.5.0    httpuv_1.5.4       Matrix_1.2-18      plyr_1.8.6         pkgconfig_2.0.3   
[25] devtools_2.3.2     purrr_0.3.4        xtable_1.8-4       scales_1.1.1       processx_3.4.4     later_1.1.0.1     
[31] TeachingDemos_2.12 tibble_3.0.3       mgcv_1.8-31        generics_0.0.2     ggplot2_3.3.2      usethis_1.6.3     
[37] ellipsis_0.3.1     withr_2.3.0        cli_2.0.2          magrittr_1.5       crayon_1.3.4       mime_0.9          
[43] memoise_1.1.0      ps_1.3.4           fs_1.5.0           fansi_0.4.1        doParallel_1.0.15  nlme_3.1-148      
[49] MASS_7.3-51.6      truncnorm_1.0-8    SuppDists_1.1-9.5  pkgbuild_1.1.0     tools_4.0.2        prettyunits_1.1.1 
[55] lifecycle_0.2.0    stringr_1.4.0      munsell_0.5.0      plotrix_3.7-8      rngtools_1.5       ggsci_2.9         
[61] callr_3.4.4        compiler_4.0.2     contfrac_1.1-12    rlang_0.4.7        grid_4.0.2         iterators_1.0.12  
[67] rstudioapi_0.11    abtest_0.2.1       gtable_0.3.0       codetools_0.2-16   deSolve_1.28       reshape2_1.4.4    
[73] R6_2.4.1           gridExtra_2.3      dplyr_1.0.2        fastmap_1.0.1      rprojroot_1.3-2    desc_1.2.0        
[79] stringi_1.5.3      BFDA_0.5.0         parallel_4.0.2     Rcpp_1.0.5         vctrs_0.3.4        tidyselect_1.1.0
```

