### Functions to extract results and parameters from MATCH reports ####

MATCH_extractRoulette <- function(MATCHtxt){

  # Left bin edges
  BinLeft <- gsub('\"', "", simplify2array(strsplit(MATCHtxt[1], '\",')))
  BinLeftMin1 <- as.numeric(gsub("]", "", BinLeft[2:10]))
  BinLeft1 <- as.numeric(gsub("Bin Left Edges: \\[", "", BinLeft[1]))

  # Number of chips in bins
  Chips <- gsub('\"', "", simplify2array(strsplit(MATCHtxt[2], '\",')))
  ChipsMin1 <- as.numeric(gsub("]", "", Chips[2:10]))
  Chips1 <- as.numeric(gsub("Chips: \\[", "", Chips[1]))

  # Min-Max Range
  minVal <- as.numeric(gsub("Minimum: ", "", MATCHtxt[3]))
  maxVal <- as.numeric(gsub("Maximum: ", "", MATCHtxt[4]))

  return(c(BinLeft1, BinLeftMin1,
           Chips1, ChipsMin1,
           minVal, maxVal))

}

MATCH_extract_tTest <- function(filePath){

  res <- data.frame(matrix(NA, nrow = length(filePath), ncol = 28))

  for(i in seq_along(filePath)){

    # Read in data from MATCH txt file
    MATCHtxt <- readLines(filePath[i])[c(3:6,9:10, 15)]

    # Roulette method
    roulette <- MATCH_extractRoulette(MATCHtxt)

    # Normal Prior Parameters
    normalPS <- simplify2array(strsplit(MATCHtxt[5], '; '))[-3]
    normalMu <- as.numeric(gsub("Normal: \u3bc = ", "", normalPS[1]))
    normalSigma <- as.numeric(gsub("\u3c3 = ", "", normalPS[2]))

    # t Prior Parameters
    tPS <- simplify2array(strsplit(MATCHtxt[6], '; '))[-4]
    tMu <- as.numeric(gsub("Student-t: \u3bc = ", "", tPS[1]))
    tScale <- as.numeric(gsub("\u3c3 = ", "", tPS[2]))
    tdf <- as.numeric(gsub("\u3bd = ", "", tPS[3]))

    # Best Fit
    BestFit <- gsub("Best Fit: ", "", simplify2array(strsplit(MATCHtxt[7], '; ')))

    # Define results object
    res[i,] <- c(roulette, normalMu, normalSigma, tMu, tScale, tdf, BestFit)
  }

  colnames(res) <- c(paste0("BinLeft", 1:10),
                     paste0("Chips", 1:10),
                     "minVal", "maxVal",
                     "mu_norm", "sigma_norm",
                     "mu_t", "scale_t", "df_t",
                     "BestFit")

  return(res)

}

MATCH_extract_cor <- function(filePath){

  res <- data.frame(matrix(NA, nrow = length(filePath), ncol = 25))

  for(i in seq_along(filePath)){

    # Read in data from MATCH txt file
    MATCHtxt <- readLines(filePath[i])[c(3:6,11, 15)]

    # Roulette method
    roulette <- MATCH_extractRoulette(MATCHtxt)

    # Beta Prior Parameters
    betaPS <- simplify2array(strsplit(MATCHtxt[5], '; '))[-3]
    betaAlpha <- as.numeric(gsub("Scaled Beta: \u3b1 = ", "", betaPS[1]))
    betaBeta <- as.numeric(gsub("\u3b2 = ", "", betaPS[2]))

    # Best Fit
    BestFit <- gsub("Best Fit: ", "", simplify2array(strsplit(MATCHtxt[6], '; ')))

    # Define results object
    res[i,] <- c(roulette, betaAlpha, betaBeta, BestFit)
  }

  colnames(res) <- c(paste0("BinLeft", 1:10),
                     paste0("Chips", 1:10),
                     "minVal", "maxVal",
                     "beta_a", "beta_b",
                     "BestFit")

  return(res)

}
